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

import qualified Connect.AtlassianTypes             as CA
import           Connect.Instances                  ()
import qualified Connect.Tenant                     as CT
import           Control.Applicative
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

type ReminderId = Integer

data Reminder = Reminder
   { reminderReminderId           :: ReminderId
   , reminderTenantId             :: Integer
   , reminderIssueId              :: CA.IssueId
   , reminderOriginalIssueKey     :: CA.IssueKey
   , reminderIssueKey             :: CA.IssueKey
   , reminderOriginalIssueSummary :: CA.IssueSummary
   , reminderIssueSummary         :: CA.IssueSummary
   , reminderUserKey              :: CA.UserKey
   , reminderUserEmail            :: CA.UserEmail
   , reminderMessage              :: Maybe T.Text
   , reminderDate                 :: UTCTime
   } deriving (Eq,Show,Generic)

instance FromRow Reminder where
  fromRow = Reminder <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data EmailReminder = EmailReminder
   { erReminderId           :: ReminderId
   , erIssueId              :: CA.IssueId
   , erOriginalIssueKey     :: CA.IssueKey
   , erIssueKey             :: CA.IssueKey
   , erOriginalIssueSummary :: CA.IssueSummary
   , erIssueSummary         :: CA.IssueSummary
   , erUserKey              :: CA.UserKey
   , erUserEmail            :: CA.UserEmail
   , erReminderMessage      :: Maybe T.Text
   , erReminderDate         :: UTCTime
   , erTenantBaseUrl        :: URI
   } deriving (Eq, Show, Generic)

instance FromRow EmailReminder where
   fromRow = EmailReminder <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

addReminder :: Connection -> UTCTime -> Integer -> CA.IssueDetails -> CA.UserDetails -> Maybe T.Text -> IO (Maybe Integer)
addReminder conn date tenantId issueDetails userDetails message = do
  reminderID <- liftIO $ insertReturning conn
    [sql|
      INSERT INTO reminder (tenantId, issueId, originalIssueKey, issueKey, originalIssueSummary, issueSummary, userKey, userEmail, message, date)
      VALUES (?,?,?,?,?,?,?,?,?,?) RETURNING id
    |] (tenantId, iid, ikey, ikey, isum, isum, ukey, uemail, message, date)
  return . listToMaybe $ join reminderID
  where
    iid = CA.issueId issueDetails
    ikey = CA.issueKey issueDetails
    isum = CA.issueSummary issueDetails
    ukey = CA.userKey userDetails
    uemail = CA.userEmail userDetails

getReminderByUser :: CT.Tenant -> CA.UserKey -> ReminderId -> Connection -> IO (Maybe Reminder)
getReminderByUser tenant userKey pid conn = do
   result <- liftIO $ query conn
      [sql|
         SELECT id, tenantId, issueId, originalIssueKey, issueKey, originalIssueSummary, issueSummary, userKey, userEmail, message, date FROM reminder WHERE id = ? AND tenantId = ? AND userKey = ?
      |] (pid, CT.tenantId tenant, T.encodeUtf8 userKey)
   return . listToMaybe $ result

getLiveRemindersByUser :: CT.Tenant -> CA.UserKey -> Connection -> IO [Reminder]
getLiveRemindersByUser tenant userKey connection = do
   now <- getCurrentTime
   liftIO $ query connection
      [sql|
         SELECT id, tenantId, issueId, originalIssueKey, issueKey, originalIssueSummary, issueSummary, userKey, userEmail, message, date FROM reminder WHERE tenantId = ? AND userKey = ? AND date > ? ORDER BY date ASC
      |]
      (CT.tenantId tenant, T.encodeUtf8 userKey, now)

getLiveRemindersForIssueByUser :: Connection -> CT.Tenant -> CA.UserKey -> CA.IssueId -> IO [Reminder]
getLiveRemindersForIssueByUser connection tenant userKey issueId = do
   now <- getCurrentTime
   liftIO $ query connection
      [sql|
         SELECT id, tenantId, issueId, originalIssueKey, issueKey, originalIssueSummary, issueSummary, userKey, userEmail, message, date FROM reminder WHERE tenantId = ? AND issueId = ? AND userKey = ? AND date > ?
      |]
      (CT.tenantId tenant, issueId, T.encodeUtf8 userKey, now)

updateKeysForReminders :: CT.Tenant -> CA.IssueId -> CA.IssueKey -> Connection -> IO Int64
updateKeysForReminders tenant issueId newIssueKey conn = liftIO $ execute conn
   [sql|
      UPDATE reminder SET issueKey = ? WHERE tenantId = ? AND issueId = ?
   |]
   (newIssueKey, CT.tenantId tenant, issueId)

updateSummariesForReminders :: CT.Tenant -> CA.IssueId -> CA.IssueSummary -> Connection -> IO Int64
updateSummariesForReminders tenant issueId newIssueSummary conn = liftIO $ execute conn
   [sql|
      UPDATE reminder SET issueSummary = ? WHERE tenantId = ? AND issueId = ?
   |]
   (newIssueSummary, CT.tenantId tenant, issueId)

updateEmailForUser :: CT.Tenant -> CA.UserDetails -> [ReminderId] -> Connection -> IO Int64
updateEmailForUser tenant userDetails reminderIds conn = liftIO $ execute conn
   [sql|
      UPDATE reminder SET userEmail = ? WHERE tenantId = ? AND userKey = ? AND id in ?
   |]
   (CA.userEmail userDetails, CT.tenantId tenant, T.encodeUtf8 . CA.userKey $ userDetails, In reminderIds)

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

deleteReminderForUser :: CT.Tenant -> CA.UserKey -> ReminderId -> Connection -> IO Int64
deleteReminderForUser tenant userKey reminderId conn = execute conn
   [sql|
      DELETE from reminder WHERE id = ? AND tenantId = ? AND userKey = ?
   |] (reminderId, CT.tenantId tenant, T.encodeUtf8 userKey)

deleteRemindersForIssue :: CT.Tenant -> CA.IssueId -> Connection -> IO Int64
deleteRemindersForIssue tenant issueId conn = execute conn
   [sql|
      DELETE from reminder WHERE tenantId = ? AND issueId = ?
   |] (CT.tenantId tenant, issueId)

deleteManyRemindersForUser :: CT.Tenant -> [ReminderId] -> CA.UserKey -> Connection -> IO Int64
deleteManyRemindersForUser tenant reminderIds userKey conn = execute conn
   [sql|
      DELETE from reminder WHERE tenantId = ? AND userKey = ? AND id in ?
   |] (CT.tenantId tenant, userKey, In reminderIds)
