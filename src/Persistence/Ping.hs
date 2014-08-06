{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Persistence.Ping
   ( Ping(..)
   , EmailReminder(..)
   , addPing
   , getReminderByUser
   , getExpiredReminders
   , getLivePingsByUser
   , updateEmailForUser
   , getLivePingsForIssueByUser
   , deletePingForUser
   , deletePing
   , deleteManyPings
   ) where

import Data.Maybe
import Data.Time.Clock
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad
import GHC.Generics
import GHC.Int
import Persistence.PostgreSQL
import Network.URI (URI)
import qualified Persistence.Tenant as PT

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B

import qualified Connect.AtlassianTypes as CA

type PingId = Integer

data Ping = Ping
   { pingPingId         :: PingId
   , pingTenantId       :: Integer
   , pingIssueId        :: CA.IssueId
   , pingIssueKey       :: CA.IssueKey
   , pingIssueSummary   :: CA.IssueSummary
   , pingUserKey        :: CA.UserKey
   , pingUserEmail      :: CA.UserEmail
   , pingMessage        :: Maybe T.Text
   , pingDate           :: UTCTime
   } deriving (Eq,Show,Generic)

instance FromRow Ping where
  fromRow = Ping <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data EmailReminder = EmailReminder
   { erPingId           :: PingId
   , erIssueId          :: CA.IssueId
   , erIssueKey         :: CA.IssueKey
   , erIssueSummary     :: CA.IssueSummary
   , erUserKey          :: CA.UserKey
   , erUserEmail        :: CA.UserEmail
   , erPingMessage      :: Maybe T.Text
   , erPingDate         :: UTCTime
   , erTenantBaseUrl    :: URI
   } deriving (Eq, Show, Generic)

instance FromRow EmailReminder where
   fromRow = EmailReminder <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

addPing :: Connection -> UTCTime -> Integer -> CA.IssueDetails -> CA.UserDetails -> Maybe T.Text -> IO (Maybe Integer)
addPing conn date tenantId issueDetails userDetails message = do
  pingID <- liftIO $ insertReturning conn
    [sql|
      INSERT INTO ping (tenantId, issueId, issueKey, issueSummary, userKey, userEmail, message, date)
      VALUES (?,?,?,?,?,?,?,?) RETURNING id
    |] (tenantId, iid, ikey, isum, ukey, uemail, message, date)
  return . listToMaybe $ join pingID
  where
    iid = CA.issueId issueDetails
    ikey = CA.issueKey issueDetails
    isum = CA.issueSummary issueDetails
    ukey = CA.userKey userDetails
    uemail = CA.userEmail userDetails

getReminderByUser :: PT.Tenant -> CA.UserKey -> PingId -> Connection -> IO (Maybe Ping)
getReminderByUser tenant userKey pid conn = do
   result <- liftIO $ query conn
      [sql|
         SELECT id, tenantId, issueId, issueKey, issueSummary, userKey, userEmail, message, date FROM ping WHERE id = ? AND tenantId = ? AND userKey = ?
      |] (pid, PT.tenantId tenant, B.pack userKey)
   return . listToMaybe $ result

getLivePingsByUser :: PT.Tenant -> CA.UserKey -> Connection -> IO [Ping]
getLivePingsByUser tenant userKey connection = do
   now <- getCurrentTime
   liftIO $ query connection
      [sql|
         SELECT id, tenantId, issueId, issueKey, issueSummary, userKey, userEmail, message, date FROM ping WHERE tenantId = ? AND userKey = ? AND date > ? ORDER BY date ASC
      |]
      (PT.tenantId tenant, B.pack userKey, now)

getLivePingsForIssueByUser :: Connection -> PT.Tenant -> CA.UserKey -> CA.IssueId -> IO [Ping]
getLivePingsForIssueByUser connection tenant userKey issueId = do
   now <- getCurrentTime
   liftIO $ query connection
      [sql|
         SELECT id, tenantId, issueId, issueKey, issueSummary, userKey, userEmail, message, date FROM ping WHERE tenantId = ? AND issueId = ? AND userKey = ? AND date > ?
      |]
      (PT.tenantId tenant, issueId, B.pack userKey, now)

updateEmailForUser :: PT.Tenant -> CA.UserDetails -> [PingId] -> Connection -> IO Int64
updateEmailForUser tenant userDetails pingIds conn = liftIO $ execute conn
   [sql|
      UPDATE ping SET userEmail = ? WHERE tenantId = ? AND userKey = ? AND id in ?
   |]
   (CA.userEmail userDetails, PT.tenantId tenant, B.pack . CA.userKey $ userDetails, In pingIds)

getExpiredReminders :: UTCTime -> Connection -> IO [EmailReminder]
getExpiredReminders expireTime conn = liftIO $ query conn
    [sql|
      SELECT p.id, p.issueId, p.issueKey, p.issueSummary, p.userKey, p.userEmail, p.message, p.date, t.baseUrl FROM ping p, tenant t WHERE p.tenantId = t.id AND p.date < ?
    |]
    (Only expireTime)

deletePing :: PingId -> Connection -> IO Int64
deletePing pingId conn = execute conn
   [sql|
      DELETE from ping WHERE id = ?
   |] (Only pingId)

deleteManyPings :: [PingId] -> Connection -> IO Int64
deleteManyPings pingIds conn = execute conn
   [sql|
      DELETE from ping WHERE id in ?
   |] (Only . In $ pingIds)

deletePingForUser :: PT.Tenant -> CA.UserKey -> PingId -> Connection -> IO Int64
deletePingForUser tenant userKey pingId conn = execute conn
   [sql|
      DELETE from ping WHERE id = ? AND tenantId = ? AND userKey = ?
   |] (pingId, PT.tenantId tenant, B.pack userKey)
