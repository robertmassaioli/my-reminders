{-# LANGUAGE OverloadedStrings #-}

module ExpireHandlers
   ( handleExpireRequest
   , handleExpireFailingRequest
   , handleExpireImmediatelyRequest
   ) where

import           AppHelpers
import           Application
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class           (lift)
import           Control.Monad.Trans.Except
import           Data.MaybeUtil
import           EmailContext
import           Finder
import           HandlerHelpers                      (writeError)
import           Persistence.Reminder
import           SnapHelpers
import           System.FilePath                     ((</>))
import qualified AppConfig                           as CONF
import qualified Control.Arrow                       as A
import qualified Control.Exception                   as E
import qualified Control.Exception.Lifted            as EL
import qualified Model.Notify                         as N
import qualified Persistence.Tenant                  as PT
import qualified Snap.AtlassianConnect               as AC
import qualified Snap.Core                           as SC
import qualified TenantPopulator                     as TP
import qualified Text.Mustache                       as M

handleExpireRequest :: AppHandler ()
handleExpireRequest = handleMethods
   [ (SC.POST, expireForTimestamp)
   ]

handleExpireFailingRequest :: AppHandler ()
handleExpireFailingRequest = handleMethods
   [ (SC.POST, expireFailingForTimestamp)
   ]

handleExpireImmediatelyRequest :: AppHandler ()
handleExpireImmediatelyRequest = handleMethods
   [ (SC.POST, flushReminderBatch)
   ]

-- We expect that we will be given a timestamp by a trusted source; if that is no longer true then
-- this code needs to be changed.
-- TODO extra: we should check to make sure that the timestamp given is reasonably close to the
-- current timestamp (within the day) and turn it of for testing.
-- TODO Each timestamp should only be processed once. Need to ensure that this is thread safe.
expireForTimestamp :: AppHandler ()
expireForTimestamp = getKeyAndConfirm CONF.rmExpireKey . writeError . runExceptT $ do
   currentTime <- lift $ getTimestampOrCurrentTime
   connectConf <- lift $ AC.getConnect
   context <- ExceptT (m2e missingEmailTemplates <$> loadEmailContext "reminder-email" connectConf)
   expiredReminders <- lift $ getExpiredReminders currentTime
   liftIO . putStrLn $ "Expired reminders: " ++ showLength expiredReminders
   convertedExpiredReminders <- ExceptT (A.left conversionFailure <$> convertToTenant expiredReminders)
   lift $ sendReminders context convertedExpiredReminders
   return ()

flushReminderBatch :: AppHandler ()
flushReminderBatch = getKeyAndConfirm CONF.rmExpireKey . writeError . runExceptT $ do
   connectConf <- lift $ AC.getConnect
   context <- ExceptT (m2e missingEmailTemplates <$> loadEmailContext "flush-reminder-email" connectConf)
   reminderBatch <- lift $ getFlushBatch
   liftIO . putStrLn $ "Flushing reminders: " ++ showLength reminderBatch
   convertedReminderBatch <- ExceptT (A.left conversionFailure <$> convertToTenant reminderBatch)
   lift $ sendReminders context convertedReminderBatch
   return ()


conversionFailure :: String -> (Int, String)
conversionFailure e = (internalServer, "Failed to convert tenants: " <> e)

missingEmailTemplates :: (Int, String)
missingEmailTemplates = (internalServer, "Could not find the directory that contains the email templates!")

expireFailingForTimestamp :: AppHandler ()
expireFailingForTimestamp = getKeyAndConfirm CONF.rmExpireKey . writeError . runExceptT $ do
   currentTime <- lift getTimestampOrCurrentTime
   connectConf <- lift AC.getConnect
   context <- ExceptT (m2e missingEmailTemplates <$> loadEmailContext "reminder-email" connectConf)
   expiredReminders <- lift $ getExpiredFailingReminders currentTime
   liftIO . putStrLn $ "Expired failing reminders: " ++ showLength expiredReminders
   convertedExpiredReminders <- ExceptT (A.left conversionFailure <$> convertToTenant expiredReminders)
   lift $ sendReminders context convertedExpiredReminders
   return ()

convertToTenant :: [(Reminder, PT.EncryptedTenant)] -> AppHandler (Either String [(Reminder, AC.Tenant)])
convertToTenant = runExceptT . mapM convert
   where
      convert (reminder, eTenant) = do
         tenant <- ExceptT $ TP.convertTenant eTenant
         return (reminder, tenant)

loadEmailContext :: String -> AC.Connect -> AppHandler (Maybe EmailContext)
loadEmailContext fileNameNoSuffix connectConf = do
   potentialEmailDirectory <- liftIO $ findDirectory addEmailDirectory
   case potentialEmailDirectory of
      Nothing -> return Nothing
      Just emailDirectory -> do
         -- Load the templates from the filesystem
         plainTemplateResult <- liftIO $ M.localAutomaticCompile (emailDirectory </> (fileNameNoSuffix ++ ".txt"))
         htmlTemplateResult <- liftIO $ M.localAutomaticCompile (emailDirectory </> (fileNameNoSuffix ++ ".html"))
         case (plainTemplateResult, htmlTemplateResult) of
            (Right plainTemplate, Right htmlTemplate) -> do
               return . Just $ EmailContext
                  { ecConnectConf = connectConf
                  , ecPlainEmailTemplate = plainTemplate
                  , ecHtmlEmailTemplate = htmlTemplate
                  }
            _ -> return Nothing

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
