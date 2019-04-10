{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric             #-}
module TenantJWT (
  withTenant,
  withMaybeTenant
  ) where

import           Application
import           AesonHelpers           ( baseOptions, stripFieldNamePrefix )
import           Control.Applicative
import           Control.Monad          (guard, join, (<=<))
import           Data.Aeson
import           Data.Aeson.Types       (Options, defaultOptions, fieldLabelModifier)
import qualified Data.ByteString.Char8  as B
import qualified Data.CaseInsensitive   as DC
import           Data.Maybe             (isJust, listToMaybe, catMaybes)
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as T
import           GHC.Generics
import qualified Persistence.Tenant     as PT
import qualified Snap.AtlassianConnect  as AC
import qualified Snap.Core              as SC
import qualified Snap.Snaplet           as SS
import qualified SnapHelpers            as SH
import qualified Web.JWT                as J

-- TODO Should this be moved into the Atlassian connect code? Or does the app handler code make it too specific?
-- TODO Can we make it not wrap the request but instead run inside the request? That will let it be moved out.

type UnverifiedJWT = J.JWT J.UnverifiedJWT

withTenant :: (AC.TenantWithUser -> AppHandler (Maybe a)) -> AppHandler (Maybe a)
withTenant tennantApply = do
  parsed <- sequence [getJWTTokenFromParam, getJWTTokenFromAuthHeader]
  case firstRightOrLefts parsed of
    Left errors -> SH.respondWithErrors SH.badRequest errors >> return Nothing
    Right unverifiedJwt -> do
      possibleTenant <- getTenant unverifiedJwt
      case possibleTenant of
        Left result -> SH.respondPlainWithError SH.badRequest result >> return Nothing
        Right tenant -> tennantApply tenant

withMaybeTenant :: (Maybe AC.TenantWithUser -> AppHandler (Maybe a)) -> AppHandler (Maybe a)
withMaybeTenant tenantApply = do
  parsed <- sequence [getJWTTokenFromParam, getJWTTokenFromAuthHeader]
  case firstRightOrLefts parsed of
    Left _ -> tenantApply Nothing
    Right unverifiedJwt -> do
      possibleTenant <- either (const Nothing) Just <$> getTenant unverifiedJwt
      tenantApply possibleTenant

decodeByteString :: B.ByteString -> Maybe UnverifiedJWT
decodeByteString = J.decode . SH.byteStringToText

-- Standard GET requests (and maybe even POSTs) from Atlassian Connect will put the jwt header in a
-- param in either the query params or in form params. This method will extract it from either.
getJWTTokenFromParam :: SS.Handler b App (Either String UnverifiedJWT)
getJWTTokenFromParam = do
   potentialParam <- fmap decodeByteString <$> SC.getParam (B.pack "jwt")
   case join potentialParam of
      Nothing -> return . Left $ "There was no JWT param in the request"
      Just unverifiedJwt -> return . Right $ unverifiedJwt

-- Sometimes Atlassian Connect will pass the JWT token in an Authorization header in your requests
-- in this format:
-- Authorization: JWT <token>
-- This method will extract the JWT token from the Auth header if it is present.
getJWTTokenFromAuthHeader :: SS.Handler b App (Either String UnverifiedJWT)
getJWTTokenFromAuthHeader = do
   authHeader <- fmap (SC.getHeader authorizationHeaderName) SC.getRequest
   case authHeader of
      Just firstHeader -> if B.isPrefixOf jwtPrefix firstHeader
         then return $ maybe (Left "The JWT Auth header could not be parsed.") Right (decodeByteString . dropJwtPrefix $ firstHeader)
         else return . Left $ "The Authorization header did not contain a JWT token: " ++ show firstHeader
      _ -> return . Left $ "There was no Authorization header in the request."
   where
      jwtPrefix = B.pack "JWT "
      dropJwtPrefix = B.drop (B.length jwtPrefix)

authorizationHeaderName :: DC.CI B.ByteString
authorizationHeaderName = DC.mk . B.pack $ "Authorization"

firstRightOrLefts :: [Either b a] -> Either [b] a
firstRightOrLefts = flipEither . sequence . fmap flipEither

flipEither :: Either a b -> Either b a
flipEither (Left x)  = Right x
flipEither (Right x) = Left x

getTenant :: UnverifiedJWT -> AppHandler (Either String AC.TenantWithUser)
getTenant unverifiedJwt = do
  let potentialKey = getClientKey unverifiedJwt
  -- TODO collapse these cases
  case potentialKey of
    Nothing -> retError "Could not parse the JWT message."
    Just key -> do
      potentialTenant <- PT.lookupTenant normalisedClientKey
      case potentialTenant of
        Nothing -> retError $ "Could not find a tenant with that id: " ++ sClientKey
        Just unverifiedTenant ->
          case verifyTenant unverifiedTenant unverifiedJwt of
            Nothing -> retError "Invalid signature for request. Danger! Request ignored."
            Just verifiedTenant -> ret (verifiedTenant, getAccountId unverifiedJwt)
      where
        sClientKey          = show key
        normalisedClientKey = T.pack sClientKey

  where
    retError :: Monad m => x -> m (Either x y)
    retError = return . Left

    ret :: Monad m => y -> m (Either x y)
    ret = return . Right

verifyTenant :: AC.Tenant -> UnverifiedJWT -> Maybe AC.Tenant
verifyTenant tenant unverifiedJwt = do
  guard (isJust $ J.verify tenantSecret unverifiedJwt)
  pure tenant
  where
    tenantSecret = J.secret . AC.sharedSecret $ tenant

getClientKey :: J.JWT a -> Maybe J.StringOrURI
getClientKey jwt = J.iss . J.claims $ jwt

getFirstJust :: [Maybe a] -> Maybe a
getFirstJust = listToMaybe . catMaybes

-- TODO this T.pack is stupid. I think it should be J.TextOrURI. Fix Haskell JWT.
getAccountId :: J.JWT a -> Maybe T.Text
getAccountId jwt = getFirstJust $ fmap (\f -> f jwt) aaidExtractors
   where
      aaidExtractors :: [J.JWT a -> Maybe T.Text]
      aaidExtractors = [ getAaidFromContext, getSub ]

      -- After the migration to GDPR is activated, getAaidFromContext can be deleted
      getAaidFromContext :: J.JWT a -> Maybe T.Text
      getAaidFromContext = fmap (cmuAccountId . cmUser) . getContext

      getSub :: J.JWT a -> Maybe T.Text
      getSub = fmap (T.pack . show) . J.sub . J.claims

      getContext :: J.JWT a -> Maybe ContextMap
      getContext = resultToMaybe . fromJSON <=< Map.lookup (T.pack "context") . J.unregisteredClaims . J.claims

      resultToMaybe :: Result a -> Maybe a
      resultToMaybe (Success x) = Just x
      resultToMaybe _ = Nothing

data ContextMap = ContextMap
  { cmUser :: ContextMapUser
  } deriving (Show, Generic)

instance FromJSON ContextMap where
  parseJSON = genericParseJSON (baseOptions { fieldLabelModifier = stripFieldNamePrefix "cm" })

data ContextMapUser = ContextMapUser
  { cmuAccountId :: T.Text
  , cmuUserKey :: T.Text
  , cmuUsername :: T.Text
  , cmuDisplayName :: T.Text
  } deriving (Show, Generic)

instance FromJSON ContextMapUser where
  parseJSON = genericParseJSON (baseOptions { fieldLabelModifier = stripFieldNamePrefix "cmu" })
