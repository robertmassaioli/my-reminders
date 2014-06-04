module TenantJWT (withTenant) where

import           Control.Applicative
import           Control.Monad (join, guard)
import qualified Data.ByteString.Char8 as B
import           Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Snap.Core as SC
import qualified Web.JWT as J

import           Application
import qualified Persistence.PostgreSQL as PP
import qualified Persistence.Tenant as PT
import qualified SnapHelpers as SH

-- TODO instead of just returning the Tenant also return the extra information that comes in the JWT
-- token such as the user.
withTenant :: (PT.Tenant -> AppHandler ()) -> AppHandler ()
withTenant tennantApply = do
   jwtParam <- fmap decodeBytestring <$> SC.getParam (B.pack "jwt")
   case join jwtParam of
      Nothing -> do
         logErrorS missingTokenMessage
         SC.writeText . T.pack $ missingTokenMessage
         SH.respondBadRequest
      Just unverifiedJwt -> do
         possibleTenant <- getTenant unverifiedJwt
         case possibleTenant of
            Left result -> do
               logErrorS result
               SC.writeText . T.pack $ result
               SH.respondBadRequest -- TODO add the error message
            Right tenant -> tennantApply tenant
   where
      decodeBytestring = J.decode . SH.byteStringToText
      missingTokenMessage = "A jwt token is required for this request."

getTenant :: J.JWT J.UnverifiedJWT -> AppHandler (Either String PT.Tenant)
getTenant unverifiedJwt = do
   let potentialKey = getClientKey unverifiedJwt
   case potentialKey of
      Nothing -> retError "Could not parse the JWT message."
      Just key -> 
         PP.withConnection $ \conn -> do
            potentialTenant <- PT.lookupTenant conn normalisedClientKey
            case potentialTenant of
               Nothing -> retError $ "Could not find a tenant with that id: " ++ sClientKey
               Just unverifiedTenant -> 
                  case verifyTenant unverifiedTenant unverifiedJwt of
                     Nothing -> retError "Invalid signature for request. Danger! Request ignored."
                     Just verifiedTenant -> ret verifiedTenant
         where
            sClientKey          = show key
            normalisedClientKey = T.pack sClientKey

   where 
      retError :: Monad m => x -> m (Either x y)
      retError = return . Left

      ret :: Monad m => y -> m (Either x y)
      ret = return . Right

verifyTenant :: PT.Tenant -> J.JWT J.UnverifiedJWT -> Maybe PT.Tenant
verifyTenant tenant unverifiedJwt = do
   guard (isJust $ J.verify tenantSecret unverifiedJwt)
   pure tenant
   where
      tenantSecret = J.secret . PT.sharedSecret $ tenant
   
getClientKey :: J.JWT a -> Maybe J.StringOrURI
getClientKey jwt = J.iss . J.claims $ jwt

logErrorS :: String -> AppHandler ()
logErrorS = SC.logError . B.pack

