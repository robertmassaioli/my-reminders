{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}

module PingHandlers
   ( handlePings
   , executePingsHandler
   ) where

import qualified Data.Text as T 
import qualified Data.ByteString.Char8 as BSC
import qualified Snap.Core as SC
import qualified Snap.Snaplet as SS
import Data.Aeson
import Data.Maybe
import Application
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Database.PostgreSQL.Simple
import GHC.Generics
import Network.URI
import Control.Monad.IO.Class

import Persistence.PostgreSQL
import qualified Persistence.Ping as P 
import SnapHelpers
import qualified WithToken as WT
import qualified Persistence.Tenant as TN
import qualified Connect.AtlassianTypes as CA

data PingRequest = PingRequest 
   { pingUserKey      :: CA.UserKey
   , pingTimeStamp    :: UTCTime
   , pingIssueId      :: CA.IssueId
   , pingMessage      :: T.Text
   } deriving (Show, Generic)

instance FromJSON PingRequest
instance ToJSON PingRequest

handlePings :: AppHandler ()
handlePings = handleMethods 
   [ (SC.GET,     WT.tenantFromToken getPingHandler)
   , (SC.PUT,     WT.tenantFromToken addPingHandler)
   , (SC.DELETE,  WT.tenantFromToken deletePingHandler)
   ]

getPingHandler :: TN.Tenant -> AppHandler ()
getPingHandler tenant = do
   params <- SC.getQueryParams
   error "Getting an individual ping has not been implimented yet."

deletePingHandler :: TN.Tenant -> AppHandler ()
deletePingHandler = error "Deleting a ping has not been implimented yet."

addPingHandler :: TN.Tenant -> AppHandler ()
addPingHandler tenant = do
   request <- SC.readRequestBody (1024 * 10) -- TODO this magic number is crappy, improve
   let maybePing = Data.Aeson.decode request :: Maybe PingRequest
   case maybePing of
      Nothing -> respondBadRequest -- TODO better error message
      Just pingRequest -> addPing pingRequest tenant

addPing :: PingRequest -> TN.Tenant -> AppHandler ()
addPing pingRequest tenant = do
   addedPing <- SS.with db $ withConnection (addPingFromRequest pingRequest tenant)
   case addedPing of
      Just _ -> respondNoContent
      Nothing -> do
         logErrorS $ failedInsertPrefix ++ pingUserKey pingRequest
         respondInternalServer
   where
      failedInsertPrefix = "Failed to insert new ping: "

addPingFromRequest :: PingRequest -> TN.Tenant -> Connection -> IO (Maybe Integer)
addPingFromRequest pingRequest tenant conn = 
   P.addPing conn date' tenantId' link' userKey' message'
   where
      date' = pingUTCTime pingRequest
      tenantId' = TN.tenantId tenant
      link' = pingIssueId pingRequest
      userKey' = pingUserKey pingRequest
      message' = pingMessage pingRequest

pingUTCTime :: PingRequest -> UTCTime
pingUTCTime pingRequest = posixSecondsToUTCTime posixSeconds
   where
      posixSeconds = (realToFrac . utcTimeToPOSIXSeconds . pingTimeStamp $ pingRequest) / 1000.0

--Returns the pingID if ping was a success
ping :: P.Ping -> IO(Maybe Integer)
ping rq = do
  putStrLn("pinged: " ++ show rq)
  return $ Just (P.pingId rq)
  
executePingsHandler:: AppHandler ()
executePingsHandler = do 
  deleted <- executePings'
  SC.writeText (T.pack $ show deleted )
  respondWith 200
  
executePings':: AppHandler ([Integer]) 
executePings' = SS.with db $ withConnection $ \conn ->
  do
    pings <- liftIO $ P.getPings conn
    successes <- sequence $ map ping pings
    sequence $ map (P.deletePing conn) (catMaybes successes)                                     
    return $ catMaybes successes
