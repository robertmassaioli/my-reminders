{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.UserDetails
   ( getUserDetails
   , UserWithDetails(..)
   , ProductErrorResponse(..)
   ) where

import           AppConfig
import           Application
import qualified Connect.AtlassianTypes      as AT
import qualified Connect.Data                as CDT
import qualified Connect.Descriptor          as CD
import qualified Connect.Instances           as CI
import qualified Connect.Tenant              as CT
import qualified Control.Monad.IO.Class      as MI
import           Data.Aeson
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as BC
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Map                    as M (Map, fromList)
import           Data.Maybe
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
import           Web.Connect.QueryStringHash
import qualified Web.JWT                     as JWT

data UserWithDetails = UserWithDetails
   { name         :: AT.UserKey
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

getUserDetails :: AT.UserKey -> CT.Tenant -> AppHandler (Either ProductErrorResponse UserWithDetails)
getUserDetails userKey tenant = do
  currentTime <- MI.liftIO P.getPOSIXTime
  connectData <- CDT.getConnect
  rmConf <- getAppConf
  let signature = T.unpack $ generateJWTToken (CDT.connectPluginKey connectData) currentTime (CT.sharedSecret tenant) GET (CI.getURI . CT.baseUrl $ tenant) url
  MI.liftIO $ runRequest defaultManagerSettings GET url
    (  addHeader ("Accept","application/json")
    <> addHeader ("Authorization", BC.pack $ "JWT " ++ signature)
    <> setPotentialProxy (getProxyFromConf baseUrlString rmConf)
    )
    (basicResponder responder)
  where
    url :: T.Text
    url = decodeUtf8 $ userQueryUri `B.append` encodeParam userKey

    encodeParam :: T.Text -> B.ByteString
    encodeParam = urlEncode True . encodeUtf8

    userQueryUri :: B.ByteString
    userQueryUri = BC.pack $ baseUrlString ++ "/rest/api/2/user?username="

    baseUrlString = show . CT.baseUrl $ tenant

generateJWTToken :: CD.PluginKey -> P.POSIXTime -> T.Text -> StdMethod -> URI -> T.Text -> T.Text
generateJWTToken (CD.PluginKey pluginKey) currentTime sharedSecret' method' ourURL requestURL = JWT.encodeSigned algo secret' claims
  where
    algo = JWT.HS256
    secret' = JWT.secret sharedSecret'
    queryStringHash = createQueryStringHash method' ourURL requestURL

    claims = JWT.JWTClaimsSet { JWT.iss = JWT.stringOrURI pluginKey
                              , JWT.iat = JWT.intDate currentTime
                              , JWT.exp = JWT.intDate (currentTime + timeUnitToDiffTime expiryPeriod)
                              , JWT.sub = Nothing
                              , JWT.aud = Nothing
                              , JWT.nbf = Nothing
                              , JWT.jti = Nothing
                              , JWT.unregisteredClaims = M.fromList [("qsh", String $ fromJust queryStringHash)] -- TODO fromJust is horrible. Remove it's use.
                              }

    expiryPeriod :: Minute
    expiryPeriod = 3

responder :: FromJSON a => Int -> BL.ByteString -> Either ProductErrorResponse a
responder 200 body = case eitherDecode body of
   Right user -> Right user
   Left err -> Left $ ProductErrorResponse 200 (T.pack $ "Can't parse json response: " ++ show err)
responder responseCode body = Left $ ProductErrorResponse responseCode (decodeUtf8 . BL.toStrict $ body)
