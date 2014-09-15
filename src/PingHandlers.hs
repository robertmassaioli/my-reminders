{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module PingHandlers
  ( handlePings
  , handleMultiPings
  , handleUserReminders
  ) where

import qualified Data.Text as T
import qualified Snap.Core as SC
import qualified Snap.Snaplet as SS
import Data.Aeson
import Data.Aeson.Types (defaultOptions, fieldLabelModifier, Options)
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
import qualified Model.UserDetails as UD

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
  , prqIssue        :: CA.IssueDetails
  , prqUser         :: CA.UserDetails
  , prqMessage      :: Maybe T.Text
  } deriving (Show, Generic)

data ReminderResponse = ReminderResponse
   { prsPingId         :: Integer
   , prsIssueId        :: CA.IssueId
   , prsIssueKey       :: CA.IssueKey
   , prsIssueSummary   :: CA.IssueSummary
   , prsUserKey        :: CA.UserKey
   , prsUserEmail      :: String
   , prsMessage        :: Maybe T.Text
   , prsDate           :: UTCTime
   } deriving (Eq, Show, Generic)

data PingIdList = PingIdList
   { pids :: [Integer]
   } deriving (Eq, Show, Generic)

instance ToJSON PingIdList
instance FromJSON PingIdList

instance ToJSON TimeUnit
instance FromJSON TimeUnit

issueDetailsOptions :: Options
issueDetailsOptions = defaultOptions { fieldLabelModifier = drop 5 }

instance ToJSON CA.UserDetails where
   toJSON o = object 
      [ "Key" .= CA.userKey o
      , "Email" .= (BC.unpack . CA.userEmail $ o)
      ]

instance FromJSON CA.UserDetails where
   parseJSON (Object o) = do
      key <- o .: "Key"
      email <- o .: "Email"
      return CA.UserDetails 
         { CA.userKey = key
         , CA.userEmail = BC.pack email
         }
   parseJSON _ = fail "Expect the User to be a JSON object but it was not."

instance ToJSON CA.IssueDetails where
   toJSON = genericToJSON issueDetailsOptions

instance FromJSON CA.IssueDetails where
   parseJSON = genericParseJSON issueDetailsOptions

instance ToJSON PingRequest where
   toJSON = genericToJSON (defaultOptions { fieldLabelModifier = drop 3 })

instance FromJSON PingRequest where
   parseJSON (Object o) = do
      delay    <- o .:  "TimeDelay"
      unit     <- o .:  "TimeUnit"
      issue    <- o .:  "Issue"
      user     <- o .:  "User"
      message  <- o .:? "Message"
      return PingRequest
         { prqTimeDelay = delay
         , prqTimeUnit = unit
         , prqIssue = issue
         , prqUser = user
         , prqMessage = message
         }
   parseJSON _ = fail "Expect the PingRequest to be a JSON object but it was not."

instance ToJSON ReminderResponse where
   toJSON = genericToJSON (defaultOptions { fieldLabelModifier = drop 3 })

instance FromJSON ReminderResponse where
   parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 3 })

handleMultiPings :: AppHandler ()
handleMultiPings = handleMethods
   [ (SC.GET, WT.tenantFromToken getPingsForIssue)
   , (SC.DELETE, WT.tenantFromToken clearPingsForIssue)
   ]

toReminderResponse :: P.Ping -> ReminderResponse
toReminderResponse ping = ReminderResponse
   { prsPingId         = P.pingPingId ping
   , prsIssueId        = P.pingIssueId ping
   , prsIssueKey       = P.pingIssueKey ping
   , prsIssueSummary   = P.pingIssueSummary ping
   , prsUserKey        = P.pingUserKey ping
   , prsUserEmail      = BC.unpack . P.pingUserEmail $ ping
   , prsMessage        = P.pingMessage ping
   , prsDate           = P.pingDate ping
   }

standardAuthError :: AppHandler ()
standardAuthError = respondWithError unauthorised "You need to login before you query for reminders."

getPingsForIssue :: CT.ConnectTenant -> AppHandler ()
getPingsForIssue (_, Nothing) = standardAuthError
getPingsForIssue (tenant, Just userKey) = do
   potentialIssueId <- fmap (fmap (read . BC.unpack)) (SC.getQueryParam "issueId") :: AppHandler (Maybe CA.IssueId)
   case potentialIssueId of
      Just issueId -> do
         issuePings <- SS.with db $ withConnection (\connection -> P.getLivePingsForIssueByUser connection tenant userKey issueId)
         SC.writeLBS . encode . fmap toReminderResponse $ issuePings
      Nothing -> respondWithError badRequest "No issueId is passed into the get call."

clearPingsForIssue :: CT.ConnectTenant -> AppHandler ()
clearPingsForIssue _ = undefined

handleUserReminders :: AppHandler ()
handleUserReminders = handleMethods
   [ (SC.GET, WT.tenantFromToken getUserReminders)
   , (SC.POST, WT.tenantFromToken bulkUpdateUserEmails)
   , (SC.DELETE, WT.tenantFromToken bulkDeleteEmails)
   ]

getUserReminders :: CT.ConnectTenant -> AppHandler ()
getUserReminders (_, Nothing) = standardAuthError
getUserReminders (tenant, Just userKey) = do
   userReminders <- SS.with db $ withConnection (P.getLivePingsByUser tenant userKey)
   SC.writeLBS . encode . fmap toReminderResponse $ userReminders
   
bulkUpdateUserEmails :: CT.ConnectTenant -> AppHandler ()
bulkUpdateUserEmails (_, Nothing) = standardAuthError
bulkUpdateUserEmails (tenant, Just userKey) = parsePingIdListFromRequest $ \pingIds -> do
  potentialUserDetails <- UD.getUserDetails userKey tenant
  case potentialUserDetails of
    Left err -> respondWithError badRequest ("Could not communicate with the host product to get user details: " ++ (T.unpack . UD.perMessage) err)
    Right userDetails -> do
      SS.with db $ withConnection (P.updateEmailForUser tenant (userDetailsConvert userDetails) (pids pingIds))
      return ()
  where
    userDetailsConvert :: UD.UserWithDetails -> CA.UserDetails
    userDetailsConvert uwd = CA.UserDetails
      { CA.userKey = UD.name uwd
      , CA.userEmail = BC.pack $ UD.emailAddress uwd
      }

bulkDeleteEmails :: CT.ConnectTenant -> AppHandler ()
bulkDeleteEmails (_, Nothing) = standardAuthError
bulkDeleteEmails (tenant, Just userKey) = parsePingIdListFromRequest $ \pingIds -> do
   SS.with db $ withConnection (P.deleteManyPingsForUser tenant (pids pingIds) userKey)
   return ()

parsePingIdListFromRequest :: (PingIdList -> AppHandler ()) -> AppHandler ()
parsePingIdListFromRequest f = do
  request <- SC.readRequestBody (1024 * 10) -- TODO this magic number is crappy, improve
  let maybePing = eitherDecode request :: Either String PingIdList
  case maybePing of
    Left err -> respondWithError badRequest ("Could not understand the data provided: " ++ err)
    Right pingIds -> f pingIds

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
            Just ping -> SC.writeLBS . encode . toReminderResponse $ ping
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
addPing pingRequest (tenant, Just userKey) =
  if userKey /= requestUK
    then respondWithError badRequest ("Given the details for user " ++ requestUK ++ " however Atlassian Connect thinks you are " ++ userKey) 
    else do
      addedPing <- SS.with db $ withConnection (addPingFromRequest pingRequest tenant (prqUser pingRequest))
      case addedPing of
        Just _ -> respondNoContent
        Nothing -> respondWithError internalServer ("Failed to insert new reminder: " ++ userKey)
  where
    requestUK = CA.userKey . prqUser $ pingRequest

addPingFromRequest :: PingRequest -> TN.Tenant -> CA.UserDetails -> Connection -> IO (Maybe Integer)
addPingFromRequest pingRequest tenant userDetails conn = do
  currentTime <- getCurrentTime
  let date' = addUTCTime dateDiff currentTime
  P.addPing conn date' tenantId' iDetails userDetails message'
  where
    dateDiff = timeDiffForPingRequest pingRequest
    tenantId' = TN.tenantId tenant
    iDetails = prqIssue pingRequest
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
