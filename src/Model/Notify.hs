{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Notify
   ( sendIssueReminder
   ) where

import           AesonHelpers           (baseOptions, stripFieldNamePrefix)
import           Application
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Char8             as BC
import qualified Data.CaseInsensitive              as CI
import qualified Data.Map                          as M
import           Data.Monoid                       (mempty, (<>))
import qualified Data.Text                         as T
import           Data.Text.Encoding                (encodeUtf8, decodeUtf8)
import           EmailContext
import qualified Model.UserDetails                 as UD
import           EmailContent                      (reminderEmail, MessageContent(..))
import           GHC.Generics
import           Persistence.Reminder              (Reminder(..))
import qualified Snap.AtlassianConnect             as AC
import qualified Snap.AtlassianConnect.HostRequest as AC
import qualified Snap.Snaplet                      as SS

data Notification = Notification
  { nSubject :: T.Text
  , nTextBody :: T.Text
  , nHtmlBody :: T.Text
  , nTo :: NotificationRecipients
  } deriving (Show, Generic)

instance ToJSON Notification where
  toJSON = genericToJSON (baseOptions { fieldLabelModifier = stripFieldNamePrefix "n" })

data NotificationRecipients = NotificationRecipients
  { nrUsers :: [NotificationUser]
  } deriving (Show, Generic)

instance ToJSON NotificationRecipients where
  toJSON = genericToJSON (baseOptions { fieldLabelModifier = stripFieldNamePrefix "nr" })

data NotificationUser = NotificationUser
  { nuName :: Maybe T.Text
  , nuAccountId :: Maybe T.Text
  } deriving (Show, Generic)

instance ToJSON NotificationUser where
  toJSON = genericToJSON (baseOptions { fieldLabelModifier = stripFieldNamePrefix "nu" })

userFromAccountId :: T.Text -> NotificationUser
userFromAccountId accountId = NotificationUser
  { nuName = Nothing
  , nuAccountId = Just accountId
  }

getNotificationUser :: AC.Tenant -> Reminder -> AppHandler (Either AC.ProductErrorResponse NotificationUser)
getNotificationUser tenant reminder = return . Right . userFromAccountId . reminderUserAaid $ reminder

sendIssueReminder :: AC.Tenant -> EmailContext -> Reminder -> AppHandler (Either AC.ProductErrorResponse ())
sendIssueReminder tenant emailContext reminder = do
  potentialNotificationUser <- getNotificationUser tenant reminder
  case potentialNotificationUser of
    Left error -> return . Left $ error
    Right notificationUser -> do
      payload <- liftIO $ createNotification notificationUser
      errorOnContent <$> (SS.with connect $ AC.hostPostRequestExtended tenant notifyUrl [] (AC.addHeader (CI.mk "x-atlassian-force-account-id", "true") <> AC.setJson payload))
  where
    notifyUrl :: B.ByteString
    notifyUrl = B.concat ["/rest/api/3/issue/", bIssueId, "/notify"]

    bIssueId :: B.ByteString
    bIssueId = BC.pack . show . reminderIssueId $ reminder

    createNotification :: NotificationUser -> IO Notification
    createNotification notificationUser = do
      emailContent <- reminderEmail tenant emailContext reminder
      return Notification
        { nSubject = subject
        , nTextBody = decodeUtf8 . textContent $ emailContent
        , nHtmlBody = decodeUtf8 . htmlContent $ emailContent
        , nTo = NotificationRecipients [notificationUser]
        }

    subject :: T.Text
    subject = T.concat ["Reminder: [", reminderIssueKey reminder, "] ", reminderIssueSummary reminder]

errorOnContent :: Either AC.ProductErrorResponse (Maybe Value) -> Either AC.ProductErrorResponse ()
errorOnContent (Left x) = Left x
errorOnContent (Right (Just _)) = Left $ AC.ProductErrorResponse 200 "We recieved content from this request but we expected none."
errorOnContent (Right Nothing) = Right ()