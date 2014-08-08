{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.UserDetails
   ( getUserDetails
   , UserWithDetails(..)
   , ProductErrorResponse(..)
   ) where
 
import Application
import qualified Connect.AtlassianTypes as AT
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Time.Clock.POSIX as P
import Data.Maybe
import qualified Control.Monad.IO.Class as MI
import qualified Connect.Descriptor as CD
import qualified Connect.Data as CDT
import qualified Persistence.Tenant as PT
import GHC.Generics
import Network.URI
import Network.HTTP.Client
import Network.HTTP.Types
import Network.Api.Support
import qualified Data.Map as M (Map, fromList)

import qualified Web.JWT as JWT
import Web.Connect.QueryStringHash

data UserWithDetails = UserWithDetails 
   { name            :: String
   , emailAddress    :: String
   , avatarUrls      :: M.Map String String
   , displayName     :: String
   , active          :: Bool
   , timeZone        :: String
   } deriving (Show, Generic)
--TODO: parse url here

instance FromJSON UserWithDetails
instance ToJSON UserWithDetails

data ProductErrorResponse = ProductErrorResponse
   { perCode      :: Int
   , perMessage :: String
   } deriving (Show, Generic)

getUserDetails :: AT.UserKey -> PT.Tenant -> AppHandler (Either ProductErrorResponse UserWithDetails)
getUserDetails userKey tenant = do
  currentTime <- MI.liftIO P.getPOSIXTime
  connectData <- CDT.getConnect
  let signature = T.unpack $ generateJWTToken (CDT.connectPluginKey connectData) currentTime (PT.sharedSecret tenant) GET (PT.baseUrl tenant) url
  MI.liftIO $ runRequest defaultManagerSettings GET url
    (addHeader ("Accept","application/json") <> addHeader ("Authorization", BC.pack $ "JWT " ++ signature))
    (basicResponder responder)
  where
    url :: T.Text
    url = T.pack $ (show . PT.baseUrl $ tenant) ++ "/rest/api/2/user?username=" ++ userKey

generateJWTToken :: CD.PluginKey -> P.POSIXTime -> T.Text -> StdMethod -> URI -> T.Text -> T.Text
generateJWTToken (CD.PluginKey pluginKey) currentTime sharedSecret' method' ourURL requestURL = JWT.encodeSigned algo secret' claims
  where
    algo = JWT.HS256
    secret' = JWT.secret sharedSecret'
    queryStringHash = createQueryStringHash method' ourURL requestURL
    
    claims = JWT.JWTClaimsSet { JWT.iss = JWT.stringOrURI pluginKey
                              , JWT.iat = JWT.intDate currentTime
                              , JWT.exp = JWT.intDate (currentTime + expiryPeriodSeconds)
                              , JWT.sub = Nothing
                              , JWT.aud = Nothing
                              , JWT.nbf = Nothing
                              , JWT.jti = Nothing
                              , JWT.unregisteredClaims = M.fromList [("qsh", String $ fromJust queryStringHash)] -- TODO fromJust is horrible. Remove it's use.
                              }

    expiryPeriodSeconds :: Num a => a
    expiryPeriodSeconds = minutesToSeconds 3

minutesToSeconds :: (Num a) => a -> a
minutesToSeconds = (*) 60

responder :: FromJSON a => Int -> BL.ByteString -> Either ProductErrorResponse a
responder 200 body = case eitherDecode body of
   Right user -> Right user
   Left err -> Left $ ProductErrorResponse 200 ("Can't parse json response: " ++ show err)
responder responseCode body = Left $ ProductErrorResponse responseCode (BC.unpack . BL.toStrict $ body)
