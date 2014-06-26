{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Persistence.Ping 
   ( Ping(..)
   , addPing
   , getExpiredPings
   , getLivePingsByUser
   , getLivePingsForIssueByUser
   , deletePingForUser
   ) where

import Data.Maybe
import Data.Time.Clock
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad
import GHC.Generics
import GHC.Int
import Network.URI hiding (query)
import Persistence.PostgreSQL
import qualified Persistence.Tenant as PT

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B

import qualified Connect.AtlassianTypes as CA

data Ping = Ping 
   { pingId   :: Integer
   , pingTenantId :: Integer
   , pingIssueId  :: CA.IssueId
   , pingUserKey  :: CA.UserKey
   , pingMessage  :: Maybe T.Text
   , pingDate     :: UTCTime 
   } deriving (Eq,Show,Generic)

instance FromRow Ping where
  fromRow = Ping <$> field <*> field <*> field <*> field <*> field <*> field
  
--instance FromField URI where
--  fromField _ (Just bstr) = pure $ fromMaybe nullURI $ parseURI (B.unpack bstr)
--  fromField f _       = returnError ConversionFailed f "data is not a valid URI value"

--instance ToField URI where
--  toField = Escape . B.pack . show
 
addPing :: Connection -> UTCTime -> Integer -> CA.IssueId -> CA.UserKey -> Maybe T.Text -> IO (Maybe Integer)
addPing conn date tenantId issueId userKey message = do 
  pingID <- liftIO $ insertReturning conn 
    [sql|
      INSERT INTO ping (tenantId, issueId, userKey, message, date) 
      VALUES (?,?,?,?,?) RETURNING id
    |] (tenantId, issueId, userKey, message, date)
  return $ listToMaybe $ join pingID
  
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

getExpiredPings :: Connection -> IO [Ping]
getExpiredPings conn = do 
  now <- getCurrentTime
  liftIO $ query conn 
    [sql|
      SELECT id,tenantId,issueId,userKey,message,date FROM ping WHERE date < ? 
    |]
    (Only now)

deletePingForUser :: PT.Tenant -> CA.UserKey -> Integer -> Connection -> IO Int64
deletePingForUser tenant userKey pingId conn = execute conn 
   [sql| 
      DELETE from ping WHERE id = ? AND tenantId = ? AND userKey = ? 
   |] (pingId, PT.tenantId tenant, B.pack userKey)
