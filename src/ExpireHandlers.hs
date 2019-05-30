{-# LANGUAGE OverloadedStrings #-}

module ExpireHandlers
   ( handleExpireRequest
   , handleExpireFailingRequest
   ) where

import qualified AppConfig                           as CONF
import           AppHelpers
import           Application
import           Control.Applicative                 ((<$>))
import           Control.Concurrent.ParallelIO.Local
import qualified Control.Exception                   as E
import qualified Control.Exception.Lifted            as EL
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
import           Persistence.Reminder
import qualified Snap.AtlassianConnect               as AC
import qualified Snap.AtlassianConnect.HostRequest   as AC
import qualified Snap.Core                           as SC
import           SnapHelpers
import           System.FilePath                     ((</>))

handleExpireRequest :: AppHandler ()
handleExpireRequest = handleMethods
   [ (SC.POST, expireForTimestamp)
   ]

handleExpireFailingRequest :: AppHandler ()
handleExpireFailingRequest = handleMethods
   [ (SC.POST, expireFailingForTimestamp)
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
   potentialEmailContext <- loadEmailContext connectConf
   case potentialEmailContext of
      Nothing -> error "Could not find the directory that contains the email templates!"
      Just context -> do
         expiredReminders <- getExpiredReminders timestamp
         liftIO . putStrLn $ "Expired reminders: " ++ showLength expiredReminders
         sendReminders context expiredReminders
         return ()

expireFailingForTimestamp :: AppHandler ()
expireFailingForTimestamp = getKeyAndConfirm CONF.rmExpireKey $ do
   currentTime <- getTimestampOrCurrentTime
   rmConf <- CONF.getAppConf
   connectConf <- AC.getConnect
   expireFailingUsingTimestamp currentTime rmConf connectConf

expireFailingUsingTimestamp :: UTCTime -> CONF.AppConf -> AC.Connect -> AppHandler ()
expireFailingUsingTimestamp timestamp rmConf connectConf = do
   potentialEmailContext <- loadEmailContext connectConf
   case potentialEmailContext of
      Nothing -> error "Could not find the directory that contains the email templates!"
      Just context -> do
         expiredReminders <- getExpiredFailingReminders timestamp
         liftIO . putStrLn $ "Expired reminders: " ++ showLength expiredReminders
         sendReminders context expiredReminders
         return ()

loadEmailContext :: AC.Connect -> AppHandler (Maybe EmailContext)
loadEmailContext connectConf = do
   potentialEmailDirectory <- liftIO $ findDirectory addEmailDirectory
   case potentialEmailDirectory of
      Nothing -> return Nothing
      Just emailDirectory -> do
         -- Load the templates from the filesystem
         plainTemplate <- liftIO $ T.readFile (emailDirectory </> "reminder-email.txt")
         htmlTemplate <- liftIO $ T.readFile (emailDirectory </> "reminder-email.html")
         return . Just $ EmailContext
            { ecConnectConf = connectConf
            , ecPlainEmailTemplate = plainTemplate
            , ecHtmlEmailTemplate = htmlTemplate
            }

showLength :: [a] -> String
showLength = show . length

addEmailDirectory :: FilePath -> FilePath
addEmailDirectory f = f </> "static" </> "email"

sendReminders :: EmailContext -> [(Reminder, AC.Tenant)] -> AppHandler ()
sendReminders context reminders = do
   sequence (fmap send (take 30 reminders))
   return ()
   where
      send = safeSendReminder context

-- TODO: For now we just catch everything but in the future we might choose to be more
-- selective...maybe.
exceptionFilter :: E.SomeException -> Maybe E.SomeException
exceptionFilter = Just

simpleCatch :: AppHandler a -> AppHandler (Either E.SomeException a)
simpleCatch = EL.tryJust exceptionFilter

safeSendReminder :: EmailContext -> (Reminder, AC.Tenant) -> AppHandler ()
safeSendReminder content rt = do
   simpleCatch $ sendReminder content rt
   return ()

sendReminder :: EmailContext -> (Reminder, AC.Tenant) -> AppHandler ()
sendReminder context rt@(reminder, tenant) = do
   emailResponse <- simpleCatch $ N.sendIssueReminder tenant context reminder
   if isSuccess emailResponse || reminderSendAttempts reminder >= 10
      then removeSentReminders [rt] >> return ()
      else incrementSendAttempts rt >> return ()

isSuccess :: Either a (Either b c) -> Bool
isSuccess (Right (Right _)) = True
isSuccess _ = False

removeSentReminders :: [(Reminder, AC.Tenant)] -> AppHandler Bool
removeSentReminders sent = do
   deletedCount <- deleteManyReminders reminderIds
   return $ deletedCount == fromIntegral (length reminderIds)
   where
      reminderIds = reminderReminderId . fst <$> sent
