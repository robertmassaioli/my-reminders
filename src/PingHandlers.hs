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
import           SnapHelpers
import qualified WithToken as WT
import qualified Persistence.Tenant as TN
import qualified Connect.AtlassianTypes as CA
import qualified Connect.Tenant as CT

type TimeDelay = Integer

-- No Month time because month's are not always the same length. Approximate months as four weeks.
data TimeUnit
   = Minute
   | Hour
   | Day
   | Week
   | Year
   deriving (Show, Eq, Generic)

data PingRequest = PingRequest
  { prqTimeDelay    :: TimeDelay
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

instance ToJSON PingRequest where
   toJSON = genericToJSON (defaultOptions { fieldLabelModifier = drop 3 })

instance FromJSON PingRequest where
   parseJSON (Object o) = do
      delay    <- o .:  "TimeDelay"
      unit     <- o .:  "TimeUnit"
      issue    <- o .:  "IssueId"
      message  <- o .:? "Message"
      return PingRequest
         { prqTimeDelay = delay
         , prqTimeUnit = unit
         , prqIssueId = issue
         , prqMessage = message
         }
   parseJSON _ = fail "Expect the PingRequest to be a JSON object but it was not."

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
   , prsPingId = P.pingPingId ping
   , prsIssueId = P.pingIssueId ping
   , prsMessage = P.pingMessage ping
   }

getPingsForIssue :: CT.ConnectTenant -> AppHandler ()
getPingsForIssue (_, Nothing) = respondWithError unauthorised "You need to login before you query for reminders."
getPingsForIssue (tenant, Just userKey) = do
   potentialIssueId <- fmap (fmap (read . BC.unpack)) (SC.getQueryParam "issueId") :: AppHandler (Maybe CA.IssueId)
   case potentialIssueId of
      Just issueId -> do
         issuePings <- SS.with db $ withConnection (\connection -> P.getLivePingsForIssueByUser connection tenant userKey issueId)
         SC.writeLBS . encode . fmap toPingResponse $ issuePings
      Nothing -> respondWithError badRequest "No issueId is passed into the get call."

clearPingsForIssue :: CT.ConnectTenant -> AppHandler ()
clearPingsForIssue _ = undefined

handlePings :: AppHandler ()
handlePings = handleMethods
  [ (SC.GET,     WT.tenantFromToken getPingHandler)
  , (SC.PUT,     WT.tenantFromToken addPingHandler)
  , (SC.DELETE,  WT.tenantFromToken deletePingHandler)
  ]

getPingHandler :: CT.ConnectTenant -> AppHandler ()
getPingHandler (_, Nothing) = respondWithError unauthorised "You need to be logged in before making a request for reminders."
getPingHandler (tenant, Just userKey) = do
   rawPingId <- SC.getQueryParam "reminderId"
   let potentialPingId = fmap (read . BC.unpack) rawPingId :: Maybe Integer
   case potentialPingId of
      Just pingId -> do
         potentialPing <- SS.with db $ withConnection (P.getReminderByUser tenant userKey pingId)
         case potentialPing of
            Nothing -> respondNotFound
            Just ping -> SC.writeLBS . encode . toPingResponse $ ping
      Nothing -> respondWithError badRequest "reminderId not found, please pass the reminderId in the request. Do not know which reminder to lookup."

deletePingHandler :: CT.ConnectTenant -> AppHandler ()
deletePingHandler (_, Nothing) = respondWithError unauthorised "You need to be logged in before making a request for reminders."
deletePingHandler (tenant, Just userKey) = do
   potentialRawReminderId <- SC.getPostParam "reminderId"
   let potentialReminderId = fmap (read . BC.unpack) potentialRawReminderId :: Maybe Integer
   case potentialReminderId of
      Just reminderId -> do
         deletedPings <- SS.with db $ withConnection (P.deletePingForUser tenant userKey reminderId)
         case deletedPings of
            1 -> respondNoContent
            0 -> respondNotFound
            _ -> do
               logErrorS $ "Deleted " ++ show deletedPings ++ " reminders with a single delete request. Primary key invalid on: " ++ show reminderId
               respondInternalServer
      Nothing -> respondWithError badRequest "A reminderId is required to see which reminder should be deleted."

addPingHandler :: CT.ConnectTenant -> AppHandler ()
addPingHandler ct = do
  request <- SC.readRequestBody (1024 * 10) -- TODO this magic number is crappy, improve
  let maybePing = eitherDecode request :: Either String PingRequest
  case maybePing of
    Left err -> respondWithError badRequest err
    Right pingRequest -> addPing pingRequest ct

addPing :: PingRequest -> CT.ConnectTenant -> AppHandler ()
addPing _ (_, Nothing) = respondWithError unauthorised "You need to be logged in so that you can create a reminder. That way the reminder is against your account."
addPing pingRequest (tenant, Just userKey) = do
  addedPing <- SS.with db $ withConnection (addPingFromRequest pingRequest tenant userKey)
  case addedPing of
    Just _ -> respondNoContent
    Nothing -> respondWithError internalServer (failedInsertPrefix ++ userKey)
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

toDiffTime :: TimeDelay -> TimeUnit -> NominalDiffTime
toDiffTime mag unit = case unit of
   Minute   -> fromIntegral $ mag * 60
   Hour     -> toDiffTime (mag * 60)   Minute
   Day      -> toDiffTime (mag * 24)   Hour
   Week     -> toDiffTime (mag * 7)    Day
   Year     -> toDiffTime (mag * 365)  Day
