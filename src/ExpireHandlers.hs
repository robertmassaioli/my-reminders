{-# LANGUAGE OverloadedStrings #-}

module ExpireHandlers
   ( handleExpireRequest
   ) where

import qualified AppConfig                           as CONF
import           AppHelpers
import           Application
import           Control.Applicative                 ((<$>))
import           Control.Concurrent.ParallelIO.Local
import           Control.Monad.IO.Class
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Char8               as BSC
import qualified Data.Text                           as T
import qualified Data.Text.IO                        as T
import           Data.Time.Clock                     (UTCTime)
import qualified Model.Notify                         as N
import           Database.PostgreSQL.Simple
import           EmailContent
import           EmailContext
import           Finder
import           Mail.Hailgun
import           Persistence.PostgreSQL              (withConnection)
import           Persistence.Reminder
import qualified Snap.AtlassianConnect               as AC
import qualified Snap.AtlassianConnect.HostRequest   as AC
import qualified Snap.Core                           as SC
import qualified Snap.Snaplet                        as SS
import           SnapHelpers
import           System.FilePath                     ((</>))

handleExpireRequest :: AppHandler ()
handleExpireRequest = handleMethods
   [ (SC.POST, expireForTimestamp)
   ]

-- We expect that we will be given a timestamp by a trusted source; if that is no longer true then
-- this code needs to be changed.
-- TODO extra: we should check to make sure that the timestamp given is reasonably close to the
-- current timestamp (within the day) and turn it of for testing.
-- TODO Each timestamp should only be processed once. Need to ensure that this is thread safe.
expireForTimestamp :: AppHandler ()
expireForTimestamp = getKeyAndConfirm CONF.rmExpireKey $ do
   currentTime <- getTimestampOrCurrentTime
   rmConf <- CONF.getAppConf
   connectConf <- AC.getConnect
   expireUsingTimestamp currentTime rmConf connectConf

expireUsingTimestamp :: UTCTime -> CONF.AppConf -> AC.Connect -> AppHandler ()
expireUsingTimestamp timestamp rmConf connectConf = do
   potentialEmailDirectory <- liftIO $ findDirectory addEmailDirectory
   case potentialEmailDirectory of
      Nothing -> error "Could not find the directory that contains the email templates!"
      Just emailDirectory -> do
         expiredReminders <- getExpiredReminders timestamp
         -- Load the templates from the filesystem
         plainTemplate <- liftIO $ T.readFile (emailDirectory </> "reminder-email.txt")
         htmlTemplate <- liftIO $ T.readFile (emailDirectory </> "reminder-email.html")
         -- load the required attachments for every single email
         attachments <- liftIO $ loadAttachments [emailDirectory </> "ios7-stopwatch-outline.png"]
         liftIO . putStrLn $ "Expired reminders: " ++ showLength expiredReminders
         let context = EmailContext
                           { ecConnectConf = connectConf
                           , ecAppConf = rmConf
                           , ecPlainEmailTemplate = plainTemplate
                           , ecHtmlEmailTemplate = htmlTemplate
                           , ecAttachments = attachments
                           }
         sentReminders <- sendReminders context expiredReminders
         liftIO . putStrLn $ "Sent reminders: " ++ showLength sentReminders
         removeSentReminders sentReminders
         return ()

showLength :: [a] -> String
showLength = show . length

addEmailDirectory :: FilePath -> FilePath
addEmailDirectory f = f </> "static" </> "email"

loadAttachments :: [FilePath] -> IO [Attachment]
loadAttachments = mapM loadAttachment

loadAttachment :: FilePath -> IO Attachment
loadAttachment filepath = do
   fileContents <- B.readFile filepath
   return Attachment
      { attachmentFilePath = filepath
      , attachmentBody = AttachmentBS fileContents
      }

-- Performance: In my local testing with a Mailgun sandbox account I have seen that it takes
-- approximately 1.6s for every 10 emails that you send. You pay an extra 1.6s for every 10 emails.
-- With these numbers we can send 1875 emails in 5 minutes and implies a maximum throughput of
-- 540000 emails / day. This is a massive number of emails and would mean that:
-- 1. Our plugin is insanely popular.
-- 2. We would be in Mailgun's upper tier at ~ 16 million reminders a month.
-- I don't think this will happen instantly so this performs well enough for now and we can monitor
-- it going into the future.
sendReminders :: EmailContext -> [(Reminder, AC.Tenant)] -> AppHandler [(Reminder, AC.Tenant)]
sendReminders context reminders =
   -- fmap fst <$> filter snd <$> withPool 10 (`parallel` fmap send reminders)
   fmap fst <$> filter snd <$> sequence (fmap send reminders)
   where
      send = sendReminder context

sendReminder :: EmailContext -> (Reminder, AC.Tenant) -> AppHandler ((Reminder, AC.Tenant), Bool)
sendReminder context rt@(reminder, tenant) = do
   emailResponse <- N.sendIssueReminder tenant context reminder
   case emailResponse of
      Left errorMessage -> do
         liftIO . putStrLn $ "Error sending email: " ++ (T.unpack . AC.perMessage $ errorMessage)
         return (rt, False)
      Right _ -> return (rt, True)

removeSentReminders :: [(Reminder, AC.Tenant)] -> AppHandler Bool
removeSentReminders sent = do
   deletedCount <- deleteManyReminders reminderIds
   return $ deletedCount == fromIntegral (length reminderIds)
   where
      reminderIds = reminderReminderId . fst <$> sent
