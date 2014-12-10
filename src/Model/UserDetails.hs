{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.UserDetails
   ( getUserDetails
   , UserWithDetails(..)
   , ProductErrorResponse(..)
   ) where

import           AppConfig
import           Application
import qualified Control.Monad.IO.Class      as MI
import           Data.Aeson
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as BC
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Connect.Descriptor     as CD
import qualified Data.Map                    as M (Map, fromList)
import           Data.Monoid
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import qualified Data.Time.Clock.POSIX       as P
import           Data.Time.Units             (Minute)
import           Data.TimeUnitUTC            (timeUnitToDiffTime)
import           GHC.Generics
import           Network.Api.Support
import           Network.HTTP.Client
import           Network.HTTP.Types
import           Network.URI
import           NetworkHelpers
import qualified Snap.AtlassianConnect       as AC
import qualified Snap.Snaplet                as SS
import           Web.Connect.QueryStringHash
import qualified Web.JWT                     as JWT
import Control.Monad.State (get)

data UserWithDetails = UserWithDetails
   { name         :: AC.UserKey
   , emailAddress :: String
   , avatarUrls   :: M.Map String String
   , displayName  :: String
   , active       :: Bool
   , timeZone     :: String
   } deriving (Show, Generic)
--TODO: parse url here

instance FromJSON UserWithDetails
instance ToJSON UserWithDetails

type HttpResponseCode = Int

data ProductErrorResponse = ProductErrorResponse
   { perCode    :: HttpResponseCode
   , perMessage :: T.Text
   } deriving (Show, Generic)

getUserDetails :: AC.Tenant -> AC.UserKey -> AppHandler (Either ProductErrorResponse UserWithDetails)
getUserDetails tenant userKey = do
    rmConf <- getAppConf
    SS.with connect $ makeGetRequest tenant usernameUrl (proxyUpdate rmConf)
  where
    proxyUpdate rmConf = setPotentialProxy (getProxyFromConf productBaseUrlString rmConf)
    productBaseUrlString = show . AC.getURI . AC.baseUrl $ tenant

    usernameUrl :: T.Text
    usernameUrl = "/rest/api/2/user?username=" `T.append` encodeParam userKey

encodeParam :: T.Text -> T.Text
encodeParam = decodeUtf8 . urlEncode True . encodeUtf8

-- TODO extract everything below this line to the Atlassian Connect Core Library

-- TODO we cannot rely upon AppHandler, just AtlassianConnect. Which means that we should extract out the endo for the proxy
makeGetRequest :: FromJSON a => AC.Tenant -> T.Text -> Endo Request -> SS.Handler b AC.Connect (Either ProductErrorResponse a)
makeGetRequest tenant productRelativeUrl requestModifications = do
    currentTime <- MI.liftIO P.getPOSIXTime
    pluginKey <- fmap AC.connectPluginKey get
    case generateJWTToken pluginKey currentTime (AC.sharedSecret tenant) GET productBaseUrl url of
        Nothing -> return . Left $ ProductErrorResponse 500 "Failed to generate a JWT token to make the request."
        (Just signature) -> MI.liftIO $ runRequest defaultManagerSettings GET url
            (  addHeader ("Accept","application/json")
            <> addHeader ("Authorization", jwtPrefix `B.append` encodeUtf8 signature)
            <> requestModifications
            )
            (basicResponder responder)
    where
        jwtPrefix :: B.ByteString
        jwtPrefix = BC.pack "JWT "

        url = T.pack productBaseUrlString `T.append` productRelativeUrl
        productBaseUrlString = show productBaseUrl
        productBaseUrl = AC.getURI . AC.baseUrl $ tenant

generateJWTToken :: CD.PluginKey -> P.POSIXTime -> T.Text -> StdMethod -> URI -> T.Text -> Maybe T.Text
generateJWTToken pluginKey fromTime sharedSecret' method' ourURL requestURL = do
  queryStringHash <- createQueryStringHash method' ourURL requestURL
  return $ JWT.encodeSigned JWT.HS256 (JWT.secret sharedSecret') (createClaims pluginKey fromTime queryStringHash)

createClaims :: CD.PluginKey -> P.POSIXTime -> T.Text -> JWT.JWTClaimsSet
createClaims (CD.PluginKey pluginKey) fromTime queryStringHash = JWT.JWTClaimsSet
    { JWT.iss = JWT.stringOrURI pluginKey
    , JWT.iat = JWT.intDate fromTime
    , JWT.exp = JWT.intDate expiryTime
    , JWT.sub = Nothing
    , JWT.aud = Nothing
    , JWT.nbf = Nothing
    , JWT.jti = Nothing
    , JWT.unregisteredClaims = M.fromList [("qsh", String queryStringHash)] -- TODO fromJust is horrible. Remove it's use.
    }
    where
        expiryTime :: P.POSIXTime
        expiryTime = fromTime + timeUnitToDiffTime expiryPeriod

        -- Our default expiry period when talking to the host product directly
        expiryPeriod :: Minute
        expiryPeriod = 1

responder :: FromJSON a => Int -> BL.ByteString -> Either ProductErrorResponse a
responder 200 body = case eitherDecode body of
   Right jsonResponse -> Right jsonResponse
   Left err -> Left $ ProductErrorResponse 200 (T.pack $ "Could not parse the json response: " ++ show err)
responder responseCode body = Left $ ProductErrorResponse responseCode (decodeUtf8 . BL.toStrict $ body)
