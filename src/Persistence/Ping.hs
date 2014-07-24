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
   , pingIssueSubject   :: CA.IssueSubject
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
   , erIssueSubject     :: CA.IssueSubject
   , erUserKey          :: CA.UserKey
   , erUserEmail        :: CA.UserEmail
   , erPingMessage      :: Maybe T.Text
   , erPingDate         :: UTCTime
   , erTenantBaseUrl    :: URI
   } deriving (Eq, Show, Generic)

instance FromRow EmailReminder where
   fromRow = EmailReminder <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

addPing :: Connection -> UTCTime -> Integer -> CA.IssueId -> CA.UserKey -> Maybe T.Text -> IO (Maybe Integer)
addPing conn date tenantId issueId userKey message = do
  pingID <- liftIO $ insertReturning conn
    [sql|
      INSERT INTO ping (tenantId, issueId, userKey, message, date)
      VALUES (?,?,?,?,?) RETURNING id
    |] (tenantId, issueId, userKey, message, date)
  return . listToMaybe $ join pingID

getReminderByUser :: PT.Tenant -> CA.UserKey -> PingId -> Connection -> IO (Maybe Ping)
getReminderByUser tenant userKey pid conn = do
   result <- liftIO $ query conn
      [sql|
         SELECT id, tenantId, issueId, userKey, message, date FROM ping WHERE id = ? AND tenantId = ? AND userKey = ?
      |] (pid, PT.tenantId tenant, B.pack userKey)
   return . listToMaybe $ result

getLivePingsByUser :: Connection -> PT.Tenant -> CA.UserKey -> IO [Ping]
getLivePingsByUser connection tenant userKey = do
   now <- getCurrentTime
   liftIO $ query connection
      [sql|
         SELECT id, tenantId, issueId, userKey, message, date FROM ping WHERE tenantId = ? AND userKey = ? AND date > ?
      |]
      (PT.tenantId tenant, B.pack userKey, now)

getLivePingsForIssueByUser :: Connection -> PT.Tenant -> CA.UserKey -> CA.IssueId -> IO [Ping]
getLivePingsForIssueByUser connection tenant userKey issueId = do
   now <- getCurrentTime
   liftIO $ query connection
      [sql|
         SELECT id, tenantId, issueId, userKey, message, date FROM ping WHERE tenantId = ? AND issueId = ? AND userKey = ? AND date > ?
      |]
      (PT.tenantId tenant, issueId, B.pack userKey, now)

getExpiredReminders :: UTCTime -> Connection -> IO [EmailReminder]
getExpiredReminders expireTime conn = liftIO $ query conn
    [sql|
      SELECT p.id, p.issueid, p.issuekey, p.issuesubject, p.userkey, p.useremail, p.message, p.date, t.baseurl FROM ping p, tenant t WHERE p.tenantid = t.id AND p.date < ?
    |]
    (Only expireTime)

deletePing :: PingId -> Connection -> IO Int64
deletePing pingId conn = execute conn
   [sql|
      DELETE from ping WHERE id = ?
   |] (Only pingId)

deleteManyPings :: [PingId] -> Connection -> IO Int64
deleteManyPings pingIds conn = executeMany conn
   [sql|
      DELETE from ping WHERE id = ?
   |] (fmap Only pingIds)

deletePingForUser :: PT.Tenant -> CA.UserKey -> PingId -> Connection -> IO Int64
deletePingForUser tenant userKey pingId conn = execute conn
   [sql|
      DELETE from ping WHERE id = ? AND tenantId = ? AND userKey = ?
   |] (pingId, PT.tenantId tenant, B.pack userKey)
