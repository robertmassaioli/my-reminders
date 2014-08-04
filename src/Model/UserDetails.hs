{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.UserDetails
   ( getUserDetails
   , UserWithDetails(..)
   , MyError(..)
   , Params(..)
   ) where
 
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Time.Clock.POSIX as P
import Data.Maybe
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

data MyError = MyError 
   { code      :: Int
   , myMessage :: String
   } deriving (Show, Generic)

-- TODO Replace with a connect tenant
data Params = Params 
   { jiraBaseURL  :: String
   , hostURL      :: URI
   , username     :: String
   , sharedSecret :: T.Text
   }

--TODO: get the current time here, since we're in IO anyways
getUserDetails:: Params -> IO (Either MyError UserWithDetails)
getUserDetails params = do
  currentTime <- P.getPOSIXTime
  let signature = T.unpack $ generateJWTToken currentTime (sharedSecret params) GET (hostURL params) (T.pack url)
  runRequest defaultManagerSettings GET (T.pack url)
    (addHeader ("Accept","application/json") <> addHeader ("Authorization", BC.pack $ "JWT " ++ signature))
    (basicResponder responder)
  where
    url = jiraBaseURL params ++ "/rest/api/2/user?username=" ++ username params

generateJWTToken :: P.POSIXTime -> T.Text -> StdMethod -> URI -> T.Text -> T.Text
generateJWTToken currentTime sharedSecret' method' ourURL requestURL = JWT.encodeSigned algo secret' claims
  where
    algo = JWT.HS256
    secret' = JWT.secret sharedSecret'
    queryStringHash = createQueryStringHash method' ourURL requestURL
    
    -- TODO Some of these details are hard coded and should be derived. Like the plugin key.
    claims = JWT.JWTClaimsSet { JWT.iss = JWT.stringOrURI "com.atlassian.pingme"
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

responder :: FromJSON a => Int -> BL.ByteString -> Either MyError a
responder 200 body = f (eitherDecode body) where
  f (Right user) = Right user
  f (Left err) = Left (MyError 200 ("can't parse: " ++ show err))
responder responseCode body = Left (MyError responseCode (BC.unpack . BL.toStrict $ body))
