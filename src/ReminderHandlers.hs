{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module ReminderHandlers
  ( handleReminder
  , handleReminders
  , handleUserReminders
  ) where

import           Application
import           Control.Monad                     (void)
import           Data.Aeson
import           Data.Aeson.Types                  (Options, defaultOptions,
                                                    fieldLabelModifier)
import qualified Data.ByteString.Char8             as BC
import qualified Data.Text                         as T
import           Data.Time.Clock
import           Database.PostgreSQL.Simple
import           GHC.Generics
import qualified Model.UserDetails                 as UD
import           Persistence.PostgreSQL
import qualified Persistence.Reminder              as P
import qualified Snap.AtlassianConnect             as AC
import qualified Snap.AtlassianConnect.HostRequest as AC
import qualified Snap.Core                         as SC
import qualified Snap.Snaplet                      as SS
import           SnapHelpers
import qualified WithToken                         as WT
import qualified Text.Email.Validate as EV

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
  , prqIssue     :: AC.IssueDetails
  , prqUser      :: AC.UserDetails
  , prqMessage   :: Maybe T.Text
  } deriving (Show, Generic)

data ReminderResponse = ReminderResponse
   { prsReminderId   :: Integer
   , prsIssueId      :: AC.IssueId
   , prsIssueKey     :: AC.IssueKey
   , prsIssueSummary :: AC.IssueSummary
   , prsUserKey      :: AC.UserKey
   , prsUserEmail    :: String
   , prsMessage      :: Maybe T.Text
   , prsDate         :: UTCTime
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

instance ToJSON AC.UserDetails where
   toJSON o = object
      [ "Key" .= AC.userKey o
      , "Email" .= (BC.unpack . AC.userEmail $ o)
      ]

instance FromJSON AC.UserDetails where
   parseJSON (Object o) = do
      key <- o .: "Key"
      email <- o .: "Email"
      return AC.UserDetails
         { AC.userKey = key
         , AC.userEmail = BC.pack email
         }
   parseJSON _ = fail "Expect the User to be a JSON object but it was not."

instance ToJSON AC.IssueDetails where
   toJSON = genericToJSON issueDetailsOptions

instance FromJSON AC.IssueDetails where
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

getRemindersForIssue :: AC.TenantWithUser -> AppHandler ()
getRemindersForIssue (_, Nothing) = standardAuthError
getRemindersForIssue (tenant, Just userKey) = do
   potentialIssueId <- fmap (fmap (read . BC.unpack)) (SC.getQueryParam "issueId") :: AppHandler (Maybe AC.IssueId)
   case potentialIssueId of
      Just issueId -> do
         issueReminders <- SS.with db $ withConnection (\connection -> P.getLiveRemindersForIssueByUser connection tenant userKey issueId)
         writeJson (fmap toReminderResponse issueReminders)
      Nothing -> respondWithError badRequest "No issueId is passed into the get call."

clearRemindersForIssue :: AC.TenantWithUser -> AppHandler ()
clearRemindersForIssue _ = undefined

handleUserReminders :: AppHandler ()
handleUserReminders = handleMethods
   [ (SC.GET, WT.tenantFromToken getUserReminders)
   , (SC.POST, WT.tenantFromToken bulkUpdateUserEmails)
   , (SC.DELETE, WT.tenantFromToken bulkDeleteEmails)
   ]

getUserReminders :: AC.TenantWithUser -> AppHandler ()
getUserReminders (_, Nothing) = standardAuthError
getUserReminders (tenant, Just userKey) = do
   userReminders <- SS.with db $ withConnection (P.getLiveRemindersByUser tenant userKey)
   SC.writeLBS . encode . fmap toReminderResponse $ userReminders

bulkUpdateUserEmails :: AC.TenantWithUser -> AppHandler ()
bulkUpdateUserEmails (_, Nothing) = standardAuthError
bulkUpdateUserEmails (tenant, Just userKey) = parseReminderIdListFromRequest $ \reminderIds -> do
  potentialUserDetails <- UD.getUserDetails tenant userKey
  case potentialUserDetails of
    Left err -> respondWithError badRequest ("Could not communicate with the host product to get user details: " ++ (T.unpack . AC.perMessage) err)
    Right userDetails -> void $ withConnection (P.updateEmailForUser tenant (userDetailsConvert userDetails) (pids reminderIds))
  where
    userDetailsConvert :: UD.UserWithDetails -> AC.UserDetails
    userDetailsConvert uwd = AC.UserDetails
      { AC.userKey = UD.name uwd
      , AC.userEmail = BC.pack $ UD.emailAddress uwd
      }

bulkDeleteEmails :: AC.TenantWithUser -> AppHandler ()
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

getReminderHandler :: AC.TenantWithUser -> AppHandler ()
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

deleteReminderHandler :: AC.TenantWithUser -> AppHandler ()
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

addReminderHandler :: AC.TenantWithUser -> AppHandler ()
addReminderHandler ct = do
  request <- SC.readRequestBody (1024 * 10) -- TODO this magic number is crappy, improve
  let maybeReminder = eitherDecode request :: Either String ReminderRequest
  case maybeReminder of
    Left err -> respondWithError badRequest err
    Right reminderRequest -> addReminder reminderRequest ct

addReminder :: ReminderRequest -> AC.TenantWithUser -> AppHandler ()
addReminder _ (_, Nothing) = respondWithError unauthorised "You need to be logged in so that you can create a reminder. That way the reminder is against your account."
addReminder reminderRequest (tenant, Just userKey)
    | userKey /= requestUK = respondWithError badRequest ("Given the details for user " ++ show requestUK ++ " however Atlassian Connect thinks you are " ++ show userKey)
    | isNotValid emailAddress = respondWithError badRequest $ "Oops, your email address " ++ show emailAddress ++ " is not valid. Please change it in your profile settings."
    | otherwise = do
        addedReminder <- SS.with db $ withConnection (addReminderFromRequest reminderRequest tenant (prqUser reminderRequest))
        case addedReminder of
          Just _ -> respondNoContent
          Nothing -> respondWithError internalServer ("Failed to insert new reminder: " ++ show userKey)
  where
    isNotValid = not . EV.isValid
    requestUK = AC.userKey . prqUser $ reminderRequest
    emailAddress = AC.userEmail . prqUser $ reminderRequest

addReminderFromRequest :: ReminderRequest -> AC.Tenant -> AC.UserDetails -> Connection -> IO (Maybe Integer)
addReminderFromRequest reminderRequest tenant userDetails conn = do
  currentTime <- getCurrentTime
  let date' = addUTCTime dateDiff currentTime
  P.addReminder conn date' tenantId' iDetails userDetails message'
  where
    dateDiff = timeDiffForReminderRequest reminderRequest
    tenantId' = AC.tenantId tenant
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
