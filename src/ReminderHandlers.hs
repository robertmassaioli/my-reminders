{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module ReminderHandlers
  ( handleReminder
  , handleReminders
  , handleUserReminders
  ) where

import           Application
import           Data.Aeson
import           Data.Aeson.Types           (defaultOptions, fieldLabelModifier, Options)
import qualified Data.ByteString.Char8      as BC
import qualified Data.Text                  as T
import           Data.Time.Clock
import           Database.PostgreSQL.Simple
import           GHC.Generics
import qualified Snap.Core                  as SC
import qualified Snap.Snaplet               as SS

import qualified Connect.AtlassianTypes     as CA
import qualified Connect.Tenant             as CT
import qualified Persistence.Reminder       as P
import           Persistence.PostgreSQL
import qualified Persistence.Tenant         as TN
import           SnapHelpers
import qualified WithToken                  as WT
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

data ReminderRequest = ReminderRequest
  { prqTimeDelay :: TimeDelay
  , prqTimeUnit  :: TimeUnit
  , prqIssue     :: CA.IssueDetails
  , prqUser      :: CA.UserDetails
  , prqMessage   :: Maybe T.Text
  } deriving (Show, Generic)

data ReminderResponse = ReminderResponse
   { prsReminderId     :: Integer
   , prsIssueId        :: CA.IssueId
   , prsIssueKey       :: CA.IssueKey
   , prsIssueSummary   :: CA.IssueSummary
   , prsUserKey        :: CA.UserKey
   , prsUserEmail      :: String
   , prsMessage        :: Maybe T.Text
   , prsDate           :: UTCTime
   } deriving (Eq, Show, Generic)

data ReminderIdList = ReminderIdList
   { pids :: [Integer]
   } deriving (Eq, Show, Generic)

instance ToJSON ReminderIdList
instance FromJSON ReminderIdList

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

instance ToJSON ReminderRequest where
   toJSON = genericToJSON (defaultOptions { fieldLabelModifier = drop 3 })

instance FromJSON ReminderRequest where
   parseJSON (Object o) = do
      delay    <- o .:  "TimeDelay"
      unit     <- o .:  "TimeUnit"
      issue    <- o .:  "Issue"
      user     <- o .:  "User"
      message  <- o .:? "Message"
      return ReminderRequest
         { prqTimeDelay = delay
         , prqTimeUnit = unit
         , prqIssue = issue
         , prqUser = user
         , prqMessage = message
         }
   parseJSON _ = fail "Expect the ReminderRequest to be a JSON object but it was not."

instance ToJSON ReminderResponse where
   toJSON = genericToJSON (defaultOptions { fieldLabelModifier = drop 3 })

instance FromJSON ReminderResponse where
   parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 3 })

handleReminders :: AppHandler ()
handleReminders = handleMethods
   [ (SC.GET, WT.tenantFromToken getRemindersForIssue)
   , (SC.DELETE, WT.tenantFromToken clearRemindersForIssue)
   ]

toReminderResponse :: P.Reminder -> ReminderResponse
toReminderResponse reminder = ReminderResponse
   { prsReminderId     = P.reminderReminderId reminder
   , prsIssueId        = P.reminderIssueId reminder
   , prsIssueKey       = P.reminderIssueKey reminder
   , prsIssueSummary   = P.reminderIssueSummary reminder
   , prsUserKey        = P.reminderUserKey reminder
   , prsUserEmail      = BC.unpack . P.reminderUserEmail $ reminder
   , prsMessage        = P.reminderMessage reminder
   , prsDate           = P.reminderDate reminder
   }

standardAuthError :: AppHandler ()
standardAuthError = respondWithError unauthorised "You need to login before you query for reminders."

getRemindersForIssue :: CT.ConnectTenant -> AppHandler ()
getRemindersForIssue (_, Nothing) = standardAuthError
getRemindersForIssue (tenant, Just userKey) = do
   potentialIssueId <- fmap (fmap (read . BC.unpack)) (SC.getQueryParam "issueId") :: AppHandler (Maybe CA.IssueId)
   case potentialIssueId of
      Just issueId -> do
         issueReminders <- SS.with db $ withConnection (\connection -> P.getLiveRemindersForIssueByUser connection tenant userKey issueId)
         writeJson (fmap toReminderResponse issueReminders)
      Nothing -> respondWithError badRequest "No issueId is passed into the get call."

clearRemindersForIssue :: CT.ConnectTenant -> AppHandler ()
clearRemindersForIssue _ = undefined

handleUserReminders :: AppHandler ()
handleUserReminders = handleMethods
   [ (SC.GET, WT.tenantFromToken getUserReminders)
   , (SC.POST, WT.tenantFromToken bulkUpdateUserEmails)
   , (SC.DELETE, WT.tenantFromToken bulkDeleteEmails)
   ]

getUserReminders :: CT.ConnectTenant -> AppHandler ()
getUserReminders (_, Nothing) = standardAuthError
getUserReminders (tenant, Just userKey) = do
   userReminders <- SS.with db $ withConnection (P.getLiveRemindersByUser tenant userKey)
   SC.writeLBS . encode . fmap toReminderResponse $ userReminders
   
bulkUpdateUserEmails :: CT.ConnectTenant -> AppHandler ()
bulkUpdateUserEmails (_, Nothing) = standardAuthError
bulkUpdateUserEmails (tenant, Just userKey) = parseReminderIdListFromRequest $ \reminderIds -> do
  potentialUserDetails <- UD.getUserDetails userKey tenant
  case potentialUserDetails of
    Left err -> respondWithError badRequest ("Could not communicate with the host product to get user details: " ++ (T.unpack . UD.perMessage) err)
    Right userDetails -> do
      SS.with db $ withConnection (P.updateEmailForUser tenant (userDetailsConvert userDetails) (pids reminderIds))
      return ()
  where
    userDetailsConvert :: UD.UserWithDetails -> CA.UserDetails
    userDetailsConvert uwd = CA.UserDetails
      { CA.userKey = UD.name uwd
      , CA.userEmail = BC.pack $ UD.emailAddress uwd
      }

bulkDeleteEmails :: CT.ConnectTenant -> AppHandler ()
bulkDeleteEmails (_, Nothing) = standardAuthError
bulkDeleteEmails (tenant, Just userKey) = parseReminderIdListFromRequest $ \reminderIds -> do
   SS.with db $ withConnection (P.deleteManyRemindersForUser tenant (pids reminderIds) userKey)
   return ()

parseReminderIdListFromRequest :: (ReminderIdList -> AppHandler ()) -> AppHandler ()
parseReminderIdListFromRequest f = do
  request <- SC.readRequestBody (1024 * 10) -- TODO this magic number is crappy, improve
  let maybeReminder = eitherDecode request :: Either String ReminderIdList
  case maybeReminder of
    Left err -> respondWithError badRequest ("Could not understand the data provided: " ++ err)
    Right reminderIds -> f reminderIds

handleReminder :: AppHandler ()
handleReminder = handleMethods
  [ (SC.GET,     WT.tenantFromToken getReminderHandler)
  , (SC.PUT,     WT.tenantFromToken addReminderHandler)
  , (SC.DELETE,  WT.tenantFromToken deleteReminderHandler)
  ]

getReminderHandler :: CT.ConnectTenant -> AppHandler ()
getReminderHandler (_, Nothing) = respondWithError unauthorised "You need to be logged in before making a request for reminders."
getReminderHandler (tenant, Just userKey) = do
   rawReminderId <- SC.getQueryParam "reminderId"
   let potentialReminderId = fmap (read . BC.unpack) rawReminderId :: Maybe Integer
   case potentialReminderId of
      Just reminderId -> do
         potentialReminder <- SS.with db $ withConnection (P.getReminderByUser tenant userKey reminderId)
         case potentialReminder of
            Nothing -> respondNotFound
            Just reminder -> writeJson (toReminderResponse reminder)
      Nothing -> respondWithError badRequest "reminderId not found, please pass the reminderId in the request. Do not know which reminder to lookup."

deleteReminderHandler :: CT.ConnectTenant -> AppHandler ()
deleteReminderHandler (_, Nothing) = respondWithError unauthorised "You need to be logged in before making a request for reminders."
deleteReminderHandler (tenant, Just userKey) = do
   potentialRawReminderId <- SC.getPostParam "reminderId"
   let potentialReminderId = fmap (read . BC.unpack) potentialRawReminderId :: Maybe Integer
   case potentialReminderId of
      Just reminderId -> do
         deletedReminders <- SS.with db $ withConnection (P.deleteReminderForUser tenant userKey reminderId)
         case deletedReminders of
            1 -> respondNoContent
            0 -> respondNotFound
            _ -> do
               logErrorS $ "Deleted " ++ show deletedReminders ++ " reminders with a single delete request. Primary key invalid on: " ++ show reminderId
               respondInternalServer
      Nothing -> respondWithError badRequest "A reminderId is required to see which reminder should be deleted."

addReminderHandler :: CT.ConnectTenant -> AppHandler ()
addReminderHandler ct = do
  request <- SC.readRequestBody (1024 * 10) -- TODO this magic number is crappy, improve
  let maybeReminder = eitherDecode request :: Either String ReminderRequest
  case maybeReminder of
    Left err -> respondWithError badRequest err
    Right reminderRequest -> addReminder reminderRequest ct

addReminder :: ReminderRequest -> CT.ConnectTenant -> AppHandler ()
addReminder _ (_, Nothing) = respondWithError unauthorised "You need to be logged in so that you can create a reminder. That way the reminder is against your account."
addReminder reminderRequest (tenant, Just userKey) =
  if userKey /= requestUK
    then respondWithError badRequest ("Given the details for user " ++ requestUK ++ " however Atlassian Connect thinks you are " ++ userKey)
    else do
      addedReminder <- SS.with db $ withConnection (addReminderFromRequest reminderRequest tenant (prqUser reminderRequest))
      case addedReminder of
        Just _ -> respondNoContent
        Nothing -> respondWithError internalServer ("Failed to insert new reminder: " ++ userKey)
  where
    requestUK = CA.userKey . prqUser $ reminderRequest

addReminderFromRequest :: ReminderRequest -> TN.Tenant -> CA.UserDetails -> Connection -> IO (Maybe Integer)
addReminderFromRequest reminderRequest tenant userDetails conn = do
  currentTime <- getCurrentTime
  let date' = addUTCTime dateDiff currentTime
  P.addReminder conn date' tenantId' iDetails userDetails message'
  where
    dateDiff = timeDiffForReminderRequest reminderRequest
    tenantId' = TN.tenantId tenant
    iDetails = prqIssue reminderRequest
    message' = prqMessage reminderRequest

timeDiffForReminderRequest :: ReminderRequest -> NominalDiffTime
timeDiffForReminderRequest request = toDiffTime (prqTimeDelay request) (prqTimeUnit request)

toDiffTime :: TimeDelay -> TimeUnit -> NominalDiffTime
toDiffTime mag unit = case unit of
   Minute   -> fromIntegral $ mag * 60
   Hour     -> toDiffTime (mag * 60)   Minute
   Day      -> toDiffTime (mag * 24)   Hour
   Week     -> toDiffTime (mag * 7)    Day
   Year     -> toDiffTime (mag * 365)  Day
