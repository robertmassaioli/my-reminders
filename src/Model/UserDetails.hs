
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.UserDetails
   ( getUserDetails
   , UserWithDetails(..)
   , canViewIssue
   ) where

import           Application
import           AesonHelpers                      ( stripFieldNamePrefix )
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Aeson.Types                  (defaultOptions, fieldLabelModifier)
import qualified Data.ByteString                   as B
import qualified Data.Map                          as M
import           Data.Monoid                       (mempty)
import qualified Data.Text                         as T
import           Data.Text.Encoding                (encodeUtf8)
import           GHC.Generics
import qualified Snap.AtlassianConnect             as AC
import qualified Snap.AtlassianConnect.HostRequest as AC
import qualified Snap.Snaplet                      as SS

data UserWithDetails = UserWithDetails
   { name         :: AC.UserKey
   , accountId    :: T.Text
   , avatarUrls   :: M.Map String String
   , displayName  :: String
   , active       :: Bool
   , timeZone     :: String
   } deriving (Show, Generic)
--TODO: parse url here

instance FromJSON UserWithDetails
instance ToJSON UserWithDetails

getUserDetails :: AC.Tenant -> AC.UserKey -> AppHandler (Either AC.ProductErrorResponse UserWithDetails)
getUserDetails tenant userKey = SS.with connect $ AC.hostGetRequest tenant Nothing usernameUrl queryParams mempty
   where
      usernameUrl :: B.ByteString
      usernameUrl = "/rest/api/2/user"

      queryParams :: [(B.ByteString, Maybe B.ByteString)]
      queryParams = [("key", Just . encodeUtf8 $ userKey)]

canViewIssue :: AC.Tenant -> AC.UserKey -> AC.IssueId -> AppHandler (Either AC.ProductErrorResponse Bool)
canViewIssue tenant userKey issueId = runExceptT $ do
   response <- ExceptT . SS.with connect $ AC.hostPostRequest tenant Nothing permissionsCheckUrl [] (body)
   case (bpgProjectPermissions response) of
      [bppg] -> case (bppgIssues bppg) of
         [returnedIssueId] -> return . all id $
            [ returnedIssueId == issueId
            , "EDIT_ISSUES" == bppgPermission bppg
            ]
         _ -> return False
      _ -> return False
   where
      permissionsCheckUrl :: B.ByteString
      permissionsCheckUrl = "/rest/api/3/permissions/check"

      body = AC.setJson $ toRequestBody userKey issueId

{-
{
  "accountId": "5b10a2844c20165700ede21g",
  "projectPermissions": [
    {
      "permissions": [
        "EDIT_ISSUES"
      ],
      "issues": [
        10010,
        10011,
        10012,
        10013,
        10014
      ]
    }
  ]
}
-}

data BulkPermissionGrants = BulkPermissionGrants
   { bpgProjectPermissions :: [BulkProjectPermissionGrants]
   , bpgGlobalPermissions :: [T.Text]
   } deriving (Eq, Show, Generic)

instance FromJSON BulkPermissionGrants where
   parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = stripFieldNamePrefix "bpg" })

data BulkProjectPermissionGrants = BulkProjectPermissionGrants
   { bppgPermission :: T.Text
   , bppgIssues :: [Integer]
   , bppgProjects :: [Integer]
   } deriving (Eq, Show, Generic)

instance FromJSON BulkProjectPermissionGrants where
   parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = stripFieldNamePrefix "bppg" })

toRequestBody :: AC.UserKey -> AC.IssueId -> Value
toRequestBody userKey issueId = object
   [ "accountId" .= userKey
   , "projectPermissions" .=
      [ object
         [ "permissions" .= ["EDIT_ISSUES" :: T.Text]
         , "issues" .= [issueId]
         ]
      ]
   ]
