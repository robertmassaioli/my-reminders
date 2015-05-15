{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Persistence.Reminder
   ( Reminder(..)
   , EmailReminder(..)
   , addReminder
   , getReminderByUser
   , getExpiredReminders
   , getLiveRemindersByUser
   , updateKeysForReminders
   , updateSummariesForReminders
   , updateEmailForUser
   , getLiveRemindersForIssueByUser
   , deleteReminderForUser
   , deleteReminder
   , deleteRemindersForIssue
   , deleteManyReminders
   , deleteManyRemindersForUser
   ) where

import           Control.Applicative                ((<$>), (<*>))
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Maybe
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           Data.Time.Clock
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Generics
import           GHC.Int
import           Network.URI                        (URI)
import           Persistence.Instances              ()
import           Persistence.PostgreSQL
import qualified Snap.AtlassianConnect              as AC

type ReminderId = Integer

data Reminder = Reminder
   { reminderReminderId           :: ReminderId
   , reminderTenantId             :: Integer
   , reminderIssueId              :: AC.IssueId
   , reminderOriginalIssueKey     :: AC.IssueKey
   , reminderIssueKey             :: AC.IssueKey
   , reminderOriginalIssueSummary :: AC.IssueSummary
   , reminderIssueSummary         :: AC.IssueSummary
   , reminderUserKey              :: AC.UserKey
   , reminderUserEmail            :: AC.UserEmail
   , reminderMessage              :: Maybe T.Text
   , reminderDate                 :: UTCTime
   } deriving (Eq,Show,Generic)

instance FromRow Reminder where
  fromRow = Reminder <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data EmailReminder = EmailReminder
   { erReminderId           :: ReminderId
   , erIssueId              :: AC.IssueId
   , erOriginalIssueKey     :: AC.IssueKey
   , erIssueKey             :: AC.IssueKey
   , erOriginalIssueSummary :: AC.IssueSummary
   , erIssueSummary         :: AC.IssueSummary
   , erUserKey              :: AC.UserKey
   , erUserEmail            :: AC.UserEmail
   , erReminderMessage      :: Maybe T.Text
   , erReminderDate         :: UTCTime
   , erTenantBaseUrl        :: URI
   } deriving (Eq, Show, Generic)

instance FromRow EmailReminder where
   fromRow = EmailReminder <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

addReminder :: Connection -> UTCTime -> Integer -> AC.IssueDetails -> AC.UserDetails -> Maybe T.Text -> IO (Maybe Integer)
addReminder conn date tenantId issueDetails userDetails message = do
  reminderID <- liftIO $ insertReturning conn
    [sql|
      INSERT INTO reminder (tenantId, issueId, originalIssueKey, issueKey, originalIssueSummary, issueSummary, userKey, userEmail, message, date)
      VALUES (?,?,?,?,?,?,?,?,?,?) RETURNING id
    |] (tenantId, iid, ikey, ikey, isum, isum, ukey, uemail, message, date)
  return . listToMaybe $ join reminderID
  where
    iid = AC.issueId issueDetails
    ikey = AC.issueKey issueDetails
    isum = AC.issueSummary issueDetails
    ukey = AC.userKey userDetails
    uemail = AC.userEmail userDetails

getReminderByUser :: AC.Tenant -> AC.UserKey -> ReminderId -> Connection -> IO (Maybe Reminder)
getReminderByUser tenant userKey pid conn = do
   result <- liftIO $ query conn
      [sql|
         SELECT id, tenantId, issueId, originalIssueKey, issueKey, originalIssueSummary, issueSummary, userKey, userEmail, message, date FROM reminder WHERE id = ? AND tenantId = ? AND userKey = ?
      |] (pid, AC.tenantId tenant, T.encodeUtf8 userKey)
   return . listToMaybe $ result

getLiveRemindersByUser :: AC.Tenant -> AC.UserKey -> Connection -> IO [Reminder]
getLiveRemindersByUser tenant userKey connection = do
   now <- getCurrentTime
   liftIO $ query connection
      [sql|
         SELECT id, tenantId, issueId, originalIssueKey, issueKey, originalIssueSummary, issueSummary, userKey, userEmail, message, date FROM reminder WHERE tenantId = ? AND userKey = ? AND date > ? ORDER BY date ASC
      |]
      (AC.tenantId tenant, T.encodeUtf8 userKey, now)

getLiveRemindersForIssueByUser :: Connection -> AC.Tenant -> AC.UserKey -> AC.IssueId -> IO [Reminder]
getLiveRemindersForIssueByUser connection tenant userKey issueId = do
   now <- getCurrentTime
   liftIO $ query connection
      [sql|
         SELECT id, tenantId, issueId, originalIssueKey, issueKey, originalIssueSummary, issueSummary, userKey, userEmail, message, date FROM reminder WHERE tenantId = ? AND issueId = ? AND userKey = ? AND date > ?
      |]
      (AC.tenantId tenant, issueId, T.encodeUtf8 userKey, now)

updateKeysForReminders :: AC.Tenant -> AC.IssueId -> AC.IssueKey -> Connection -> IO Int64
updateKeysForReminders tenant issueId newIssueKey conn = liftIO $ execute conn
   [sql|
      UPDATE reminder SET issueKey = ? WHERE tenantId = ? AND issueId = ?
   |]
   (newIssueKey, AC.tenantId tenant, issueId)

updateSummariesForReminders :: AC.Tenant -> AC.IssueId -> AC.IssueSummary -> Connection -> IO Int64
updateSummariesForReminders tenant issueId newIssueSummary conn = liftIO $ execute conn
   [sql|
      UPDATE reminder SET issueSummary = ? WHERE tenantId = ? AND issueId = ?
   |]
   (newIssueSummary, AC.tenantId tenant, issueId)

updateEmailForUser :: AC.Tenant -> AC.UserDetails -> [ReminderId] -> Connection -> IO Int64
updateEmailForUser tenant userDetails reminderIds conn = liftIO $ execute conn
   [sql|
      UPDATE reminder SET userEmail = ? WHERE tenantId = ? AND userKey = ? AND id in ?
   |]
   (AC.userEmail userDetails, AC.tenantId tenant, T.encodeUtf8 . AC.userKey $ userDetails, In reminderIds)

getExpiredReminders :: UTCTime -> Connection -> IO [EmailReminder]
getExpiredReminders expireTime conn = liftIO $ query conn
    [sql|
      SELECT p.id, p.issueId, p.originalIssueKey, p.issueKey, p.originalIssueSummary, p.issueSummary, p.userKey, p.userEmail, p.message, p.date, t.baseUrl FROM reminder p, tenant t WHERE p.tenantId = t.id AND p.date < ?
    |]
    (Only expireTime)

deleteReminder :: ReminderId -> Connection -> IO Int64
deleteReminder reminderId conn = execute conn
   [sql|
      DELETE from reminder WHERE id = ?
   |] (Only reminderId)

deleteManyReminders :: [ReminderId] -> Connection -> IO Int64
deleteManyReminders reminderIds conn = execute conn
   [sql|
      DELETE from reminder WHERE id in ?
   |] (Only . In $ reminderIds)

deleteReminderForUser :: AC.Tenant -> AC.UserKey -> ReminderId -> Connection -> IO Int64
deleteReminderForUser tenant userKey reminderId conn = execute conn
   [sql|
      DELETE from reminder WHERE id = ? AND tenantId = ? AND userKey = ?
   |] (reminderId, AC.tenantId tenant, T.encodeUtf8 userKey)

deleteRemindersForIssue :: AC.Tenant -> AC.IssueId -> Connection -> IO Int64
deleteRemindersForIssue tenant issueId conn = execute conn
   [sql|
      DELETE from reminder WHERE tenantId = ? AND issueId = ?
   |] (AC.tenantId tenant, issueId)

deleteManyRemindersForUser :: AC.Tenant -> [ReminderId] -> AC.UserKey -> Connection -> IO Int64
deleteManyRemindersForUser tenant reminderIds userKey conn = execute conn
   [sql|
      DELETE from reminder WHERE tenantId = ? AND userKey = ? AND id in ?
   |] (AC.tenantId tenant, userKey, In reminderIds)
