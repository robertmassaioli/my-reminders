{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.UserDetails(getUserDetails,UserWithDetails(..),MyError) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Char as C
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Maybe
import qualified Data.List as L
import GHC.Generics 
import Network.URI
import Network.HTTP.Client
import Network.HTTP.Types
import Network.Api.Support
import Data.String.Utils
import Data.Map (Map(..))

data UserWithDetails = UserWithDetails {
                               name:: String,
                               emailAddress:: String,
                               avatarUrls:: Map String String,
                               displayName:: String,
                               active:: Bool,
                               timeZone:: String
                               } 
                 deriving(Show,Generic)
--TODO: parse url here

instance FromJSON UserWithDetails

instance ToJSON UserWithDetails

data MyError = MyError {code::Int, myMessage::String} deriving(Show, Generic)

getUserDetails:: String  -> String -> ((StdMethod,String) -> String) -> IO(Either MyError UserWithDetails)
getUserDetails baseURL username signer = 
  runRequest defaultManagerSettings GET (T.pack url) 
  (
   addHeader ("Accept","application/json") <>                                                                     addHeader ("Authorization", (BS.pack $ "JWT " ++ signature))
  ) (basicResponder responder) where
    url = baseURL ++ "/rest/api/2/user?username=" ++ username
    signature = signer (GET, url)


responder :: FromJSON a => Int -> BL.ByteString -> Either MyError a
responder 200 body = f (eitherDecode body) where 
  f (Right(user)) = Right(user)
  f (Left(error)) = Left(MyError 200 ("can't parse: " ++ (show error)))
responder code body = Left(MyError code (BS.unpack $ BL.toStrict body))

parseUser:: String -> Maybe(UserWithDetails)
parseUser input = decode $ B.pack input


baseOptions :: Options
baseOptions = defaultOptions
    { omitNothingFields = True
    }

stripFieldNamePrefix :: String -> String -> String
stripFieldNamePrefix pre s = toLowerFirst $ fromMaybe s (L.stripPrefix pre s)
    where toLowerFirst (c : cs) = C.toLower c : cs
          toLowerFirst [] = []

--TODO: For testing, ignore, delete

parseUser':: String -> Either String UserWithDetails
parseUser' input = eitherDecode $ B.pack input

testuser2:: String
testuser2 = replace "'" "\"" testuser

--testUser' = test
testuser:: String
testuser = "{ \n\
\    'self': 'http://www.example.com/jira/rest/api/2/user?username=fred',\
\    'name': 'fred',\
\    'emailAddress': 'fred@example.com',\
\    'avatarUrls': {\
\        '24x24': 'http://www.example.com/jira/secure/useravatar?size=small&ownerId=fred',\
\        '16x16': 'http://www.example.com/jira/secure/useravatar?size=xsmall&ownerId=fred',\
\        '32x32': 'http://www.example.com/jira/secure/useravatar?size=medium&ownerId=fred',\
\        '48x48': 'http://www.example.com/jira/secure/useravatar?size=large&ownerId=fred'\
\    },\
\    'displayName': 'Fred F. User',\
\    'active': true,\
\    'timeZone': 'Australia/Sydney',\
\    'groups': {\
\        'size': 3,\
\        'items': [\
\            {\
\                'name': 'jira-user',\
\                'self': 'http://www.example.com/jira/rest/api/2/group?groupname=jira-user'\
\            },\
\            {\
\                'name': 'jira-admin',\
\                'self': 'http://www.example.com/jira/rest/api/2/group?groupname=jira-admin'\
\            },\
\            {\
\                'name': 'important',\
\               'self': 'http://www.example.com/jira/rest/api/2/group?groupname=important'\
\            }\
\        ]\
\    },\
\    'expand': 'groups'\
\}"


{--
testuser' = "{\
\    \"self\": \"http://www.example.com/jira/rest/api/2/user?username=fred\",\
\    \"name\": \"fred\",\
\    \"emailAddress\": \"fred@example.com\",\
\    \"avatarUrls\": {\
\        \"24x24\": \"http://www.example.com/jira/secure/useravatar?size=small&ownerId=fred\",\
\        \"16x16\": \"http://www.example.com/jira/secure/useravatar?size=xsmall&ownerId=fred\",\
\        \"32x32\": \"http://www.example.com/jira/secure/useravatar?size=medium&ownerId=fred\",\
\        \"48x48\": \"http://www.example.com/jira/secure/useravatar?size=large&ownerId=fred\"\
\    },\
\    \"displayName\": \"Fred F. User\",\
\    \"active\": true,\
\    \"timeZone\": \"Australia/Sydney\",\
\    \"groups\": {\
\        \"size\": 3,\
\        \"items\": [\
\            {\
\                \"name\": \"jira-user\",\
\                \"self\": \"http://www.example.com/jira/rest/api/2/group?groupname=jira-user\"\
\            }\
\            {\
\                \"name\": \"jira-admin\",\
\                \"self\": \"http://www.example.com/jira/rest/api/2/group?groupname=jira-admin\"\
\            },\
\            {\
\                \"name\": \"important\",\
\               \"self\": \"http://www.example.com/jira/rest/api/2/group?groupname=important\"\
\            }\
\        ]\
\    },\
\    \"expand\": \"groups\"\
\}"

--}
