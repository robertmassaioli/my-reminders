{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.UserDetails
   ( getUserDetails
   , UserWithDetails(..)
   ) where

import           AppConfig
import           Application
import           Data.Aeson
import qualified Data.Map                          as M
import qualified Data.Text                         as T
import           Data.Text.Encoding                (decodeUtf8, encodeUtf8)
import           GHC.Generics
import           Network.HTTP.Types
import           NetworkHelpers
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
getUserDetails tenant userKey = do
    rmConf <- getAppConf
    SS.with connect $ AC.makeGetRequest tenant usernameUrl (proxyUpdate rmConf)
  where
    proxyUpdate rmConf = setPotentialProxy (getProxyFromConf productBaseUrlString rmConf)
    productBaseUrlString = show . AC.getURI . AC.baseUrl $ tenant

    usernameUrl :: T.Text
    usernameUrl = "/rest/api/2/user?username=" `T.append` encodeParam userKey

encodeParam :: T.Text -> T.Text
encodeParam = decodeUtf8 . urlEncode True . encodeUtf8
