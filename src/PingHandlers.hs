{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module PingHandlers
  ( handlePings
  , handleMultiPings
  ) where

import qualified Data.Text as T 
import qualified Snap.Core as SC
import qualified Snap.Snaplet as SS
import Data.Aeson
import Data.Aeson.Types (defaultOptions, fieldLabelModifier)
import Application
import Data.Time.Clock
import qualified Data.ByteString.Char8 as BC
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
  { prqTimeDelay    :: TimeMagnitude
  , prqTimeUnit     :: TimeUnit
  , prqIssueId      :: CA.IssueId
  , prqMessage      :: Maybe T.Text
  } deriving (Show, Generic)

data PingResponse = PingResponse
   { prsDate :: UTCTime
   , prsPingId :: Integer
   , prsIssueId :: CA.IssueId
   , prsMessage :: Maybe T.Text
   } deriving (Show, Generic)

instance ToJSON TimeUnit
instance FromJSON TimeUnit

instance ToJSON PingRequest
instance FromJSON PingRequest

instance ToJSON PingResponse where
   toJSON = genericToJSON (defaultOptions { fieldLabelModifier = drop 3 })

instance FromJSON PingResponse where
   parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 3 })

handleMultiPings :: AppHandler ()
handleMultiPings = handleMethods
   [ (SC.GET, WT.tenantFromToken getPingsForIssue)
   , (SC.DELETE, WT.tenantFromToken clearPingsForIssue)
   ]

toPingResponse :: P.Ping -> PingResponse
toPingResponse ping = PingResponse
   { prsDate = P.pingDate ping
   , prsPingId = P.pingId ping
   , prsIssueId = P.pingIssueId ping
   , prsMessage = P.pingMessage ping
   }

getPingsForIssue :: CT.ConnectTenant -> AppHandler ()
getPingsForIssue (_, Nothing) = error "Tried to get the pings for an issue without logging in."
getPingsForIssue (tenant, Just userKey) = do
   potentialIssueId <- fmap (fmap (read . BC.unpack)) (SC.getQueryParam "issueId") :: AppHandler (Maybe CA.IssueId)
   case potentialIssueId of
      Just issueId -> do
         issuePings <- SS.with db $ withConnection (\connection -> P.getLivePingsForIssueByUser connection tenant userKey issueId)
         SC.writeLBS . encode . fmap toPingResponse $ issuePings
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
         { prqTimeDelay = 1
         , prqTimeUnit = Day
         , prqIssueId = 10000
         , prqMessage = Just "This is a cool message with text and stuff."
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
    link' = prqIssueId pingRequest
    message' = prqMessage pingRequest

timeDiffForPingRequest :: PingRequest -> NominalDiffTime
timeDiffForPingRequest request = toDiffTime (prqTimeDelay request) (prqTimeUnit request)

toDiffTime :: TimeMagnitude -> TimeUnit -> NominalDiffTime
toDiffTime mag unit = case unit of
   Minute   -> fromIntegral $ mag * 60
   Hour     -> toDiffTime (mag * 60)   Minute
   Day      -> toDiffTime (mag * 24)   Hour
   Week     -> toDiffTime (mag * 7)    Day
   Year     -> toDiffTime (mag * 365)  Day
