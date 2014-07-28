{-# LANGUAGE OverloadedStrings #-}

module ExpireHandlers 
   ( handleExpireRequest
   ) where

import           Application
import           Control.Applicative ((<$>))
import           Control.Concurrent.ParallelIO.Local
import qualified Data.ByteString.Char8 as BC
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX
import           Database.PostgreSQL.Simple
import           EmailContent
import           Mail.Hailgun
import           Persistence.Ping
import           Persistence.PostgreSQL (withConnection)
import qualified RemindMeConfiguration as RC
import qualified Snap.Core as SC
import qualified Snap.Snaplet as SS
import           SnapHelpers


handleExpireRequest :: AppHandler ()
handleExpireRequest = handleMethods
   [ (SC.PUT, expireForTimestamp)
   ]

-- TODO we expect that we will be given a timestamp and that we can expire everything before that
-- timestamp.
-- TODO extra: we should check to make sure that the timestamp given is reasonably close to the
-- current timestamp (within the day). 
-- TODO Each timestamp should only be processed once. Need to ensure that this is thread safe.
-- TODO a hash should need to be provided such that only those that have the hash can trigger the
-- rest calls.
expireForTimestamp :: AppHandler ()
expireForTimestamp = do
   potentialExpireKey <- SC.getQueryParam "key"
   potentialRawTimestamp <- SC.getQueryParam "timestamp"
   let potentialTimestamp = (read . BC.unpack) <$> potentialRawTimestamp :: Maybe Integer
   case (potentialExpireKey, potentialTimestamp) of
      (Nothing, _) -> respondWithError forbidden "Speak friend and enter. However: http://i.imgur.com/fVDH5bN.gif"
      (_      , Nothing) -> respondWithError badRequest "You need to provide a timestamp for expiry to work."
      (Just expireKey, Just timestamp) -> do
         rmConf <- RC.getRMConf
         if RC.rmExpireKey rmConf /= (BC.unpack expireKey)
            then respondWithError forbidden "You lack the required permissions."
            else SS.with db (withConnection $ expireUsingTimestamp (integerPosixToUTCTime timestamp) rmConf)
         -- otherwise expire the tokens that need to be expired, do so in a brand new method

integerPosixToUTCTime :: Integer -> UTCTime
integerPosixToUTCTime = posixSecondsToUTCTime . fromIntegral

expireUsingTimestamp :: UTCTime -> RC.RMConf -> Connection -> IO ()
expireUsingTimestamp timestamp rmConf conn = do
   expiredReminders <- getExpiredReminders timestamp conn
   --putStrLn $ "Expired reminders: " ++ (show . length $ expiredReminders)
   sentReminders <- sendReminders rmConf expiredReminders
   --putStrLn $ "Sent reminders: " ++ (show sentReminders)
   removeSentReminders sentReminders conn
   return ()
         
sendReminders :: RC.RMConf -> [EmailReminder] -> IO [EmailReminder]
sendReminders rmConf reminders =
   fmap fst <$> (filter snd) <$> withPool 10 (flip parallel (fmap send reminders))
   where
      send = sendReminder rmConf

sendReminder :: RC.RMConf -> EmailReminder -> IO (EmailReminder, Bool)
sendReminder rmConf reminder = do
   case pingToHailgunMessage rmConf reminder of
      Left _ -> return (reminder, False)
      Right message -> (,) reminder <$> isRight <$> sendEmail (RC.rmHailgunContext rmConf) message

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

pingToHailgunMessage :: RC.RMConf -> EmailReminder -> Either HailgunErrorMessage HailgunMessage
pingToHailgunMessage rmConf reminder = hailgunMessage subject message from recipients
   where 
      subject = "Reminder: [" ++ erIssueKey reminder ++ "] " ++ erIssueSummary reminder
      message = reminderEmail reminder
      from = RC.rmFromAddress rmConf
      recipients = emptyMessageRecipients { recipientsTo = [ erUserEmail reminder ] }

removeSentReminders :: [EmailReminder] -> Connection -> IO Bool
removeSentReminders reminders conn = do
   deletedCount <- deleteManyPings (fmap erPingId reminders) conn
   return $ deletedCount == (fromIntegral $ length reminders)
