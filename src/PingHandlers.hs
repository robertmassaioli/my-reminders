{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module PingHandlers
  ( handlePings
  ) where

import qualified Data.Text as T 
import qualified Snap.Core as SC
import qualified Snap.Snaplet as SS
import Data.Aeson
import Application
import Data.Time.Clock
import Database.PostgreSQL.Simple
import GHC.Generics

import Persistence.PostgreSQL
import qualified Persistence.Ping as P 
import SnapHelpers
import qualified WithToken as WT
import qualified Persistence.Tenant as TN
import qualified Connect.AtlassianTypes as CA
import qualified Connect.Tenant as CT

type TimeMagnitude = Integer

-- No Month time because month's are not always the same length. Approximate months as four weeks.
data TimeUnit 
   = Minute
   | Hour
   | Day
   | Week
   | Year
   deriving (Show, Eq, Generic)

data PingRequest = PingRequest 
  { prMagnitude    :: TimeMagnitude
  , prTimeUnit     :: TimeUnit
  , prIssueId      :: CA.IssueId
  , prMessage      :: Maybe T.Text -- TODO turn this into a maybe type
  } deriving (Show, Generic)

instance ToJSON TimeUnit
instance FromJSON TimeUnit

instance ToJSON PingRequest
instance FromJSON PingRequest

handleMultiPings :: AppHandler ()
handleMultiPings = handleMethods
   [ (SC.GET, WT.tenantFromToken getPingsForIssue)
   , (SC.DELETE, WT.tenantFromToken clearPingsForIssue)
   ]

getPingsForIssue :: CT.ConnectTenant -> AppHandler ()
getPingsForIssue (_, Nothing) = error "Tried to get the pings for an issue without logging in."
getPingsForIssue (tenant, Just userKey) = do
   potentialIssueId <- fmap read (SC.getQueryParam "issueId") :: AppHandler (Maybe CA.IssueId)
   case potentialIssueId of
      Just issueId -> do
         issuePings <- SS.with db $ withConnection (\connection -> P.getLivePingsForIssueByUser connection tenant userKey issueId)
         SC.writeLBS . encode $ issuePings
      Nothing -> error "No issueId is passed into the get call."

clearPingsForIssue :: CT.ConnectTenant -> AppHandler ()
clearPingsForIssue tenant = undefined

handlePings :: AppHandler ()
handlePings = handleMethods 
  [ (SC.GET,     WT.tenantFromToken getPingHandler)
  , (SC.PUT,     WT.tenantFromToken addPingHandler)
  , (SC.DELETE,  WT.tenantFromToken deletePingHandler)
  ]

getPingHandler :: CT.ConnectTenant -> AppHandler ()
getPingHandler tenant = do
   params <- SC.getQueryParams
   -- TODO it should have a valid ID
   -- TODO the user making the request should own the ping
   SC.writeLBS . encode $ exampleData
   --error "Getting an individual ping has not been implimented yet."
   where
      exampleData = PingRequest 
         { prMagnitude = 1
         , prTimeUnit = Day
         , prIssueId = 10000
         , prMessage = Just "This is a cool message with text and stuff."
         }

deletePingHandler :: CT.ConnectTenant -> AppHandler ()
deletePingHandler = error "Deleting a ping has not been implimented yet."
-- TODO the ping to delete should have a valid request and the user making the request should own
-- the ping

logMessage :: String -> AppHandler ()
logMessage message = do
  logErrorS message
  SC.writeText . T.pack $ message

addPingHandler :: CT.ConnectTenant -> AppHandler ()
addPingHandler ct = do
  logMessage "Started handling the ping..."
  request <- SC.readRequestBody (1024 * 10) -- TODO this magic number is crappy, improve
  let maybePing = Data.Aeson.decode request :: Maybe PingRequest
  case maybePing of
    Nothing -> do
      logMessage "Failed to parse the ping that we were given."
      respondBadRequest -- TODO better error message
    Just pingRequest -> addPing pingRequest ct

addPing :: PingRequest -> CT.ConnectTenant -> AppHandler ()
addPing _ (_, Nothing) = error "The user that is attempting to make the ping has not logged in."
addPing pingRequest (tenant, Just userKey) = do
  addedPing <- SS.with db $ withConnection (addPingFromRequest pingRequest tenant userKey)
  case addedPing of
    Just _ -> respondNoContent
    Nothing -> do
      logErrorS $ failedInsertPrefix ++ userKey
      respondInternalServer
    where
      failedInsertPrefix = "Failed to insert new ping: "

addPingFromRequest :: PingRequest -> TN.Tenant -> CA.UserKey -> Connection -> IO (Maybe Integer)
addPingFromRequest pingRequest tenant userKey' conn = do
  currentTime <- getCurrentTime
  let date' = addUTCTime dateDiff currentTime
  P.addPing conn date' tenantId' link' userKey' message'
  where
    dateDiff = timeDiffForPingRequest pingRequest
    tenantId' = TN.tenantId tenant
    link' = prIssueId pingRequest
    message' = prMessage pingRequest

timeDiffForPingRequest :: PingRequest -> NominalDiffTime
timeDiffForPingRequest request = toDiffTime (prMagnitude request) (prTimeUnit request)

toDiffTime :: TimeMagnitude -> TimeUnit -> NominalDiffTime
toDiffTime mag unit = case unit of
   Minute   -> fromIntegral $ mag * 60
   Hour     -> toDiffTime (mag * 60)   Minute
   Day      -> toDiffTime (mag * 24)   Hour
   Week     -> toDiffTime (mag * 7)    Day
   Year     -> toDiffTime (mag * 365)  Day
