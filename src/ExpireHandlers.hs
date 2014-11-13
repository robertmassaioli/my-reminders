{-# LANGUAGE OverloadedStrings #-}

module ExpireHandlers 
   ( handleExpireRequest
   ) where

import           Application
import           Control.Applicative ((<$>))
import           Control.Concurrent.ParallelIO.Local
import qualified Data.ByteString.Char8 as BSC
import           Data.Time.Clock (UTCTime)
import           Database.PostgreSQL.Simple
import           EmailContent
import           Mail.Hailgun
import           Persistence.Reminder
import           Persistence.PostgreSQL (withConnection)
import qualified AppConfig as CONF
import qualified Snap.Core as SC
import qualified Snap.Snaplet as SS
import           SnapHelpers


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
   SS.with db (withConnection $ expireUsingTimestamp currentTime rmConf)

expireUsingTimestamp :: UTCTime -> CONF.AppConf -> Connection -> IO ()
expireUsingTimestamp timestamp rmConf conn = do
   expiredReminders <- getExpiredReminders timestamp conn
   --putStrLn $ "Expired reminders: " ++ (show . length $ expiredReminders)
   sentReminders <- sendReminders rmConf expiredReminders
   --putStrLn $ "Sent reminders: " ++ (show sentReminders)
   removeSentReminders sentReminders conn
   return ()
         
-- Performance: In my local testing with a Mailgun sandbox account I have seen that it takes
-- approximately 1.6s for every 10 emails that you send. You pay an extra 1.6s for every 10 emails.
-- With these numbers we can send 1875 emails in 5 minutes and implies a maximum throughput of
-- 540000 emails / day. This is a massive number of emails and would mean that:
-- 1. Our plugin is insanely popular.
-- 2. We would be in Mailgun's upper tier at ~ 16 million reminders a month.
-- I don't think this will happen instantly so this performs well enough for now and we can monitor
-- it going into the future.
sendReminders :: CONF.AppConf -> [EmailReminder] -> IO [EmailReminder]
sendReminders rmConf reminders =
   fmap fst <$> filter snd <$> withPool 10 (flip parallel (fmap send reminders))
   where
      send = sendReminder rmConf

sendReminder :: CONF.AppConf -> EmailReminder -> IO (EmailReminder, Bool)
sendReminder rmConf reminder =
   case reminderToHailgunMessage rmConf reminder of
      Left _ -> return (reminder, False)
      Right message -> (,) reminder <$> isRight <$> sendEmail (CONF.rmHailgunContext rmConf) message

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

reminderToHailgunMessage :: CONF.AppConf -> EmailReminder -> Either HailgunErrorMessage HailgunMessage
reminderToHailgunMessage rmConf reminder = hailgunMessage subject message from recipients
   where 
      subject = "Reminder: [" ++ erIssueKey reminder ++ "] " ++ erIssueSummary reminder
      message = reminderEmail reminder
      from = BSC.pack $ CONF.rmFromUser rmConf `toEmailFormat` (hailgunDomain . CONF.rmHailgunContext $ rmConf)
      recipients = emptyMessageRecipients { recipientsTo = [ erUserEmail reminder ] }

toEmailFormat :: String -> String -> String
toEmailFormat from domain = from ++ "@" ++ domain

removeSentReminders :: [EmailReminder] -> Connection -> IO Bool
removeSentReminders reminders conn = do
   deletedCount <- deleteManyReminders (fmap erReminderId reminders) conn
   return $ deletedCount == fromIntegral (length reminders)
