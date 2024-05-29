{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Persistence.Reminder
   ( Reminder(..)
   , UserAaid
   , addReminder
   , getReminderByUser
   , getExpiredReminders
   , getFlushBatch
   , getExpiredFailingReminders
   , incrementSendAttempts
   , getLiveRemindersByUser
   , updateKeysForReminders
   , getUsersWithRemindersOnIssue
   , updateSummariesForReminders
   , getLiveRemindersForIssueByUser
   , deleteReminderForUser
   , deleteReminder
   , deleteRemindersForIssue
   , deleteManyReminders
   , deleteManyRemindersForUser
   ) where

import           Application
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.Time.Clock
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Generics
import           GHC.Int
import           Persistence.Instances              ()
import           Snap.Snaplet.PostgresqlSimple
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import qualified Persistence.Tenant                 as PT
import qualified Snap.AtlassianConnect              as AC

type ReminderId = Integer

type UserAaid = T.Text

data Reminder = Reminder
   { reminderReminderId           :: ReminderId
   , reminderTenantId             :: Integer
   , reminderIssueId              :: AC.IssueId
   , reminderOriginalIssueKey     :: AC.IssueKey
   , reminderIssueKey             :: AC.IssueKey
   , reminderOriginalIssueSummary :: AC.IssueSummary
   , reminderIssueSummary         :: AC.IssueSummary
   , reminderUserAaid             :: UserAaid
   , reminderMessage              :: Maybe T.Text
   , reminderDate                 :: UTCTime
   , reminderSendAttempts         :: Integer
   } deriving (Eq,Show,Generic)

instance FromRow Reminder where
  fromRow = Reminder <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

addReminder :: UTCTime -> Integer -> AC.IssueDetails -> UserAaid -> Maybe T.Text -> AppHandler (Maybe Integer)
addReminder date tenantId issueDetails aaid message =
  listToMaybe . join <$> returning
    [sql|
      INSERT INTO reminder (tenantId, issueId, originalIssueKey, issueKey, originalIssueSummary, issueSummary, userAaid, message, date)
      VALUES (?,?,?,?,?,?,?,?,?) RETURNING id
    |] [(tenantId, iid, ikey, ikey, isum, isum, aaid, message, date)]
  where
    iid = AC.issueId issueDetails
    ikey = AC.issueKey issueDetails
    isum = AC.issueSummary issueDetails

getReminderByUser :: AC.Tenant -> UserAaid -> ReminderId -> AppHandler (Maybe Reminder)
getReminderByUser tenant userAaid pid =
   listToMaybe <$> query
      [sql|
         SELECT id, tenantId, issueId, originalIssueKey, issueKey, originalIssueSummary, issueSummary, userAaid, message, date, sendAttempts
         FROM reminder
         WHERE id = ? AND tenantId = ? AND userAaid = ?
      |] (pid, AC.tenantId tenant, T.encodeUtf8 userAaid)

getLiveRemindersByUser :: AC.Tenant -> UserAaid -> AppHandler [Reminder]
getLiveRemindersByUser tenant userAaid = do
   now <- liftIO getCurrentTime
   query
      [sql|
         SELECT id, tenantId, issueId, originalIssueKey, issueKey, originalIssueSummary, issueSummary, userAaid, message, date, sendAttempts
         FROM reminder
         WHERE tenantId = ? AND userAaid = ? AND date > ? ORDER BY date ASC
      |]
      (AC.tenantId tenant, T.encodeUtf8 userAaid, now)

getLiveRemindersForIssueByUser :: AC.Tenant -> UserAaid -> AC.IssueId -> AppHandler [Reminder]
getLiveRemindersForIssueByUser tenant userAaid issueId = do
   now <- liftIO getCurrentTime
   query
      [sql|
         SELECT id, tenantId, issueId, originalIssueKey, issueKey, originalIssueSummary, issueSummary, userAaid, message, date, sendAttempts
         FROM reminder
         WHERE tenantId = ? AND issueId = ? AND userAaid = ? AND date > ?
      |]
      (AC.tenantId tenant, issueId, T.encodeUtf8 userAaid, now)

updateKeysForReminders :: AC.Tenant -> AC.IssueId -> AC.IssueKey -> AppHandler Int64
updateKeysForReminders tenant issueId newIssueKey = execute
   [sql|
      UPDATE reminder SET issueKey = ? WHERE tenantId = ? AND issueId = ?
   |]
   (newIssueKey, AC.tenantId tenant, issueId)

getUsersWithRemindersOnIssue :: AC.Tenant -> AC.IssueId -> AppHandler [AC.UserKey]
getUsersWithRemindersOnIssue tenant issueId = fmap fromOnly <$> query
   [sql|
      SELECT userAaid
      FROM reminder
      WHERE tenantId = ? AND issueId = ?
   |] (AC.tenantId tenant, issueId)

updateSummariesForReminders :: AC.Tenant -> AC.IssueId -> [AC.UserKey] -> AC.IssueSummary -> AppHandler Int64
updateSummariesForReminders tenant issueId users newIssueSummary = execute
   [sql|
      UPDATE reminder SET issueSummary = ? WHERE tenantId = ? AND issueId = ? AND userAaid in ?
   |]
   (newIssueSummary, AC.tenantId tenant, issueId, In users)

getExpiredReminders :: UTCTime -> AppHandler [(Reminder, PT.EncryptedTenant)]
getExpiredReminders expireTime = do
   results <- fmap (\(reminder :. tenant) -> (reminder, tenant)) <$> query
    [sql|
      SELECT p.id, p.tenantId, p.issueId, p.originalIssueKey, p.issueKey, p.originalIssueSummary, p.issueSummary, p.userAaid, p.message, p.date, p.sendAttempts, t.id, t.key, t.publicKey, t.oauthClientId, t.encrypted_shared_secret, t.baseUrl, t.productType
      FROM reminder p, tenant t
      WHERE p.tenantId = t.id AND p.date < ? and p.sendAttempts < 2
      ORDER BY random()
    |]
    (Only expireTime)
   return (results :: [(Reminder, PT.EncryptedTenant)])

getFlushBatch :: AppHandler [(Reminder, PT.EncryptedTenant)]
getFlushBatch = do
   results <- fmap (\(reminder :. tenant) -> (reminder, tenant)) <$> query_
    [sql|
      SELECT p.id, p.tenantId, p.issueId, p.originalIssueKey, p.issueKey, p.originalIssueSummary, p.issueSummary, p.userAaid, p.message, p.date, p.sendAttempts, t.id, t.key, t.publicKey, t.oauthClientId, t.encrypted_shared_secret, t.baseUrl, t.productType
      FROM reminder p, tenant t
      WHERE p.tenantId = t.id AND p.sendAttempts < 2
      ORDER BY random()
      LIMIT 20
    |]
   return (results :: [(Reminder, PT.EncryptedTenant)])

getExpiredFailingReminders :: UTCTime -> AppHandler [(Reminder, PT.EncryptedTenant)]
getExpiredFailingReminders expireTime = do
   results <- fmap (\(reminder :. tenant) -> (reminder, tenant)) <$> query
      [sql|
      SELECT p.id, p.tenantId, p.issueId, p.originalIssueKey, p.issueKey, p.originalIssueSummary, p.issueSummary, p.userAaid, p.message, p.date, p.sendAttempts, t.id, t.key, t.publicKey, t.oauthClientId, t.encrypted_shared_secret, t.baseUrl, t.productType
      FROM reminder p, tenant t
      WHERE p.tenantId = t.id AND p.date < ? and p.sendAttempts >= 2
      ORDER BY random()
      |]
      (Only expireTime)
   return (results :: [(Reminder, PT.EncryptedTenant)])

incrementSendAttempts :: (Reminder, AC.Tenant) -> AppHandler Int64
incrementSendAttempts (reminder, tenant) = execute
   [sql|
      UPDATE reminder SET sendAttempts = sendAttempts + 1 WHERE tenantId = ? AND id = ?
   |]
   (AC.tenantId tenant, reminderReminderId reminder)

deleteReminder :: ReminderId -> AppHandler Int64
deleteReminder reminderId = execute
   [sql|
      DELETE from reminder WHERE id = ?
   |] (Only reminderId)

deleteManyReminders :: [ReminderId] -> AppHandler Int64
deleteManyReminders reminderIds = execute
   [sql|
      DELETE from reminder WHERE id in ?
   |] (Only . In $ reminderIds)

deleteReminderForUser :: AC.Tenant -> UserAaid -> ReminderId -> AppHandler Int64
deleteReminderForUser tenant userAaid reminderId = execute
   [sql|
      DELETE from reminder WHERE id = ? AND tenantId = ? AND userAaid = ?
   |] (reminderId, AC.tenantId tenant, T.encodeUtf8 userAaid)

deleteRemindersForIssue :: AC.Tenant -> AC.IssueId -> AppHandler Int64
deleteRemindersForIssue tenant issueId = execute
   [sql|
      DELETE from reminder WHERE tenantId = ? AND issueId = ?
   |] (AC.tenantId tenant, issueId)

deleteManyRemindersForUser :: AC.Tenant -> [ReminderId] -> UserAaid -> AppHandler Int64
deleteManyRemindersForUser tenant reminderIds userAaid = execute
   [sql|
      DELETE from reminder WHERE tenantId = ? AND userAaid = ? AND id in ?
   |] (AC.tenantId tenant, userAaid, In reminderIds)
