{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Persistence.Ping 
   ( Ping(..)
   , addPing
   , getPings
   , deletePing
   ) where

import qualified Data.Text                            as T
import qualified Data.ByteString.Char8                as B
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

import qualified Connect.AtlassianTypes as CA

data Ping = Ping 
   { pingId :: Integer
   , tenantId :: Integer
   , issueId :: CA.IssueId
   , userKey :: CA.UserKey
   , message :: T.Text
   , date :: UTCTime 
   } deriving (Eq,Show,Generic)

instance FromRow Ping where
  fromRow = Ping <$> field <*> field <*> field <*> field <*> field <*> field
  
instance FromField URI where
    fromField _ (Just bstr) = pure $ fromMaybe nullURI $ parseURI (B.unpack bstr)
    fromField f _           = returnError ConversionFailed f "data is not a valid URI value"

instance ToField URI where
    toField = Escape . B.pack . show
 
addPing :: Connection -> UTCTime -> Integer -> CA.IssueId -> CA.UserKey -> T.Text -> IO (Maybe Integer)
addPing conn date tenantId issueId userKey message =
  do 
    pingID <- liftIO $ insertReturning conn 
              [sql|
               INSERT INTO ping (tenantId, issueId, userKey, message, date) 
               VALUES (?,?,?,?,?) RETURNING id
                  |] (tenantId, issueId, userKey, message, date)
    return $ listToMaybe $ join pingID
    
    
getPings:: Connection -> IO([Ping])
getPings conn = 
  do 
    now <- getCurrentTime
    pings <- liftIO $ query conn 
             [sql|
              SELECT id,tenantId,issueId,userKey,message,date FROM ping WHERE date < ? 
                 |]
             (Only now)
    return pings

deletePing:: Connection -> Integer -> IO(Int64)
deletePing conn pingId =
  execute conn [sql|DELETE FROM ping WHERE id = ?|] (Only pingId)
