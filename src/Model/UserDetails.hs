{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.UserDetails
   ( getUserDetails
   , UserWithDetails(..)
   , MyError(..)
   , Params(..)
   ) where
 
import Data.Aeson
import Data.Time.Clock (NominalDiffTime)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
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
getUserDetails:: Integer -> Params -> IO (Either MyError UserWithDetails)
getUserDetails currentTime params =
  runRequest defaultManagerSettings GET (T.pack url)
    (addHeader ("Accept","application/json") <> addHeader ("Authorization", BC.pack $ "JWT " ++ signature))
    (basicResponder responder)
  where
    url = jiraBaseURL params ++ "/rest/api/2/user?username=" ++ username params
    signature = T.unpack $ generateJWTToken currentTime (sharedSecret params) GET (hostURL params) (T.pack url)

generateJWTToken :: Integer -> T.Text -> StdMethod -> URI -> T.Text -> T.Text
generateJWTToken currentTime sharedSecret'  method' ourURL requestURL = JWT.encodeSigned algo secret' claims
  where
    algo = JWT.HS256

    -- TODO this function should not need to exist. Should accept a time in the type.
    diffTime :: Integer -> NominalDiffTime
    diffTime = fromRational . toRational
    
    queryStringHash = createQueryStringHash method' ourURL requestURL
    
    -- TODO Some of these details are hard coded and should be derived. Like the plugin key.
    claims = JWT.JWTClaimsSet { JWT.iss = JWT.stringOrURI "com.atlassian.pingme"
                              , JWT.iat = JWT.intDate $ diffTime currentTime
                              , JWT.exp = JWT.intDate $ diffTime (currentTime  + 10000000) -- TODO Generating a token that never expires is a big problem. Make it expire in 3 min.
                              , JWT.sub = Nothing
                              , JWT.aud = Nothing
                              , JWT.nbf = Nothing
                              , JWT.jti = Nothing
                              , JWT.unregisteredClaims = M.fromList [("qsh", String $ fromJust queryStringHash)] -- TODO fromJust is horrible. Remove it's use.
                              }
    
    secret' = JWT.secret sharedSecret'

responder :: FromJSON a => Int -> BL.ByteString -> Either MyError a
responder 200 body = f (eitherDecode body) where
  f (Right user) = Right user
  f (Left err) = Left (MyError 200 ("can't parse: " ++ show err))
responder responseCode body = Left (MyError responseCode (BC.unpack . BL.toStrict $ body))
