{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.UserDetails
   ( getUserDetails
   , UserWithDetails(..)
   ) where

import           Application
import           Data.Aeson
import qualified Data.ByteString                   as B
import qualified Data.Map                          as M
import           Data.Monoid                       (mempty)
import           Data.Text.Encoding                (encodeUtf8)
import           GHC.Generics
import qualified Snap.AtlassianConnect             as AC
import qualified Snap.AtlassianConnect.HostRequest as AC
import qualified Snap.Snaplet                      as SS

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

getUserDetails :: AC.Tenant -> AC.UserKey -> AppHandler (Either AC.ProductErrorResponse UserWithDetails)
getUserDetails tenant userKey = SS.with connect $ AC.hostGetRequest tenant usernameUrl queryParams mempty
  where
    usernameUrl :: B.ByteString
    usernameUrl = "/rest/api/2/user"

    queryParams :: [(B.ByteString, Maybe B.ByteString)]
    queryParams = [("username", Just . encodeUtf8 $ userKey)]
