{-# LANGUAGE OverloadedStrings #-}

module ExpireHandlers
   ( handleExpireRequest
   ) where

import qualified AppConfig                           as CONF
import           AppHelpers
import           Application
import           Control.Applicative                 ((<$>))
import           Control.Concurrent.ParallelIO.Local
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Char8               as BSC
import           Data.Either                         (isRight)
import qualified Data.Text                           as T
import qualified Data.Text.IO                        as T
import           Data.Time.Clock                     (UTCTime)
import           Database.PostgreSQL.Simple
import           EmailContent
import           EmailContext
import           Finder
import           Mail.Hailgun
import           Persistence.PostgreSQL              (withConnection)
import           Persistence.Reminder
import           Snap.AtlassianConnect               (Connect (..), getConnect)
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
   connectConf <- getConnect
   SS.with db (withConnection $ expireUsingTimestamp currentTime rmConf connectConf)

expireUsingTimestamp :: UTCTime -> CONF.AppConf -> Connect -> Connection -> IO ()
expireUsingTimestamp timestamp rmConf connectConf conn = do
   potentialEmailDirectory <- findDirectory addEmailDirectory
   case potentialEmailDirectory of
      Nothing -> error "Could not find the directory that contains the email templates!"
      Just emailDirectory -> do
         expiredReminders <- getExpiredReminders timestamp conn
         -- Load the templates from the filesystem
         plainTemplate <- T.readFile (emailDirectory </> "reminder-email.txt")
         htmlTemplate <- T.readFile (emailDirectory </> "reminder-email.html")
         -- load the required attachments for every single email
         attachments <- loadAttachments [emailDirectory </> "ios7-stopwatch-outline.png"]
         -- putStrLn $ "Expired reminders: " ++ (show . length $ expiredReminders)
         let context = EmailContext
                           { ecConnectConf = connectConf
                           , ecAppConf = rmConf
                           , ecPlainEmailTemplate = plainTemplate
                           , ecHtmlEmailTemplate = htmlTemplate
                           , ecAttachments = attachments
                           }
         sentReminders <- sendReminders context expiredReminders
         -- putStrLn $ "Sent reminders: " ++ (show sentReminders)
         removeSentReminders sentReminders conn
         return ()

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
sendReminders :: EmailContext -> [EmailReminder] -> IO [EmailReminder]
sendReminders context reminders =
   fmap fst <$> filter snd <$> withPool 10 (flip parallel (fmap send reminders))
   where
      send = sendReminder context

sendReminder :: EmailContext -> EmailReminder -> IO (EmailReminder, Bool)
sendReminder context reminder = do
   potentialMessage <- reminderToHailgunMessage context reminder
   case potentialMessage of
      Left _ -> return (reminder, False)
      Right message -> (,) reminder <$> isRight <$> sendEmail (CONF.rmHailgunContext . ecAppConf $ context) message

reminderToHailgunMessage :: EmailContext -> EmailReminder -> IO (Either HailgunErrorMessage HailgunMessage)
reminderToHailgunMessage context reminder = do
   message <- reminderEmail context reminder
   return $ hailgunMessage subject message from recipients (ecAttachments context)
   where
      appConf = ecAppConf context
      subject = T.concat ["Reminder: [", erIssueKey reminder, "] ", erIssueSummary reminder]
      from = BSC.pack $ CONF.rmFromUser appConf `toEmailFormat` (hailgunDomain . CONF.rmHailgunContext $ appConf)
      recipients = emptyMessageRecipients { recipientsTo = [ erUserEmail reminder ] }

toEmailFormat :: String -> String -> String
toEmailFormat from domain = from ++ "@" ++ domain

removeSentReminders :: [EmailReminder] -> Connection -> IO Bool
removeSentReminders reminders conn = do
   deletedCount <- deleteManyReminders (fmap erReminderId reminders) conn
   return $ deletedCount == fromIntegral (length reminders)
