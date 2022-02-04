{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module ReminderHandlers
  ( handleReminder
  , handleReminders
  , handleUserReminders
  ) where

import           AesonHelpers                      (baseOptions, stripFieldNamePrefix)
import           Application
import           Control.Monad                     (when, unless)
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.MaybeUtil
import           Data.Time.Clock
import           GHC.Generics
import           HandlerHelpers                    (writeError)
import           SnapHelpers
import qualified Data.ByteString.Char8             as BC
import qualified Data.Text                         as T
import qualified Model.UserDetails                 as HU
import qualified Persistence.Reminder              as P
import qualified Snap.AtlassianConnect             as AC
import qualified Snap.AtlassianConnect.HostRequest as AC
import qualified Snap.Core                         as SC
import qualified WithToken                         as WT

-- No Month time because month's are not always the same length. Approximate months as four weeks.
data TimeUnit
   = Minute
   | Hour
   | Day
   | Week
   | Year
   deriving (Show, Eq, Generic)

data ReminderRequest = ReminderRequest
  { prqReminderDate :: UTCTime
  , prqIssue        :: AC.IssueDetails
  , prqUserAaid     :: T.Text
  , prqMessage      :: Maybe T.Text
  } deriving (Show, Generic)

data ReminderResponse = ReminderResponse
   { prsReminderId   :: Integer
   , prsIssueId      :: AC.IssueId
   , prsIssueKey     :: AC.IssueKey
   , prsIssueSummary :: AC.IssueSummary
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
issueDetailsOptions = baseOptions { fieldLabelModifier = stripFieldNamePrefix "issue" }

instance ToJSON AC.UserDetails where
   toJSON o = object
      [ "key" .= AC.userKey o
      , "email" .= (BC.unpack . AC.userEmail $ o)
      ]

instance FromJSON AC.UserDetails where
   parseJSON (Object o) = do
      key <- o .: "key"
      email <- o .: "email"
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
   toJSON = genericToJSON (baseOptions { fieldLabelModifier = stripFieldNamePrefix "prq" })

instance FromJSON ReminderRequest where
   parseJSON = genericParseJSON (baseOptions { fieldLabelModifier = stripFieldNamePrefix "prq" })

instance ToJSON ReminderResponse where
   toJSON = genericToJSON (baseOptions { fieldLabelModifier = stripFieldNamePrefix "prs" })

instance FromJSON ReminderResponse where
   parseJSON = genericParseJSON (baseOptions { fieldLabelModifier = stripFieldNamePrefix "prs" })

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
   , prsMessage        = P.reminderMessage reminder
   , prsDate           = P.reminderDate reminder
   }

standardAuthError :: AppHandler ()
standardAuthError = respondWithError unauthorised "You need to login before you query for reminders."

getRemindersForIssue :: AC.TenantWithUser -> AppHandler ()
getRemindersForIssue (_, Nothing) = standardAuthError
getRemindersForIssue (tenant, Just userAaid) = do
   potentialIssueId <- fmap (fmap (read . BC.unpack)) (SC.getQueryParam "issueId") :: AppHandler (Maybe AC.IssueId)
   case potentialIssueId of
      Just issueId -> do
         issueReminders <- P.getLiveRemindersForIssueByUser tenant userAaid issueId
         writeJson (fmap toReminderResponse issueReminders)
      Nothing -> respondWithError badRequest "No issueId is passed into the get call."

-- TODO implement clear reminders for issue, if it needs to be implemented
clearRemindersForIssue :: AC.TenantWithUser -> AppHandler ()
clearRemindersForIssue _ = undefined

handleUserReminders :: AppHandler ()
handleUserReminders = handleMethods
   [ (SC.GET, WT.tenantFromToken getUserReminders)
   , (SC.DELETE, WT.tenantFromToken bulkDeleteEmails)
   ]

getUserReminders :: AC.TenantWithUser -> AppHandler ()
getUserReminders (_, Nothing) = standardAuthError
getUserReminders (tenant, Just userAaid) = do
   userReminders <- P.getLiveRemindersByUser tenant userAaid
   SC.writeLBS . encode . fmap toReminderResponse $ userReminders

bulkDeleteEmails :: AC.TenantWithUser -> AppHandler ()
bulkDeleteEmails (_, Nothing) = standardAuthError
bulkDeleteEmails (tenant, Just userAaid) = parseReminderIdListFromRequest $ \reminderIds -> do
   P.deleteManyRemindersForUser tenant (pids reminderIds) userAaid
   respondNoContent

parseReminderIdListFromRequest :: (ReminderIdList -> AppHandler ()) -> AppHandler ()
parseReminderIdListFromRequest f = do
  request <- SC.readRequestBody size10KB
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
getReminderHandler (tenant, Just userAaid) = do
   rawReminderId <- SC.getQueryParam "reminderId"
   let potentialReminderId = fmap (read . BC.unpack) rawReminderId :: Maybe Integer
   case potentialReminderId of
      Just reminderId -> do
         potentialReminder <- P.getReminderByUser tenant userAaid reminderId
         case potentialReminder of
            Nothing -> respondNotFound
            Just reminder -> writeJson (toReminderResponse reminder)
      Nothing -> respondWithError badRequest "reminderId not found, please pass the reminderId in the request. Do not know which reminder to lookup."

deleteReminderHandler :: AC.TenantWithUser -> AppHandler ()
deleteReminderHandler (_, Nothing) = respondWithError unauthorised "You need to be logged in before making a request for reminders."
deleteReminderHandler (tenant, Just userAaid) = do
   potentialRawReminderId <- SC.getQueryParam "reminderId"
   let potentialReminderId = fmap (read . BC.unpack) potentialRawReminderId :: Maybe Integer
   case potentialReminderId of
      Just reminderId -> do
         deletedReminders <- P.deleteReminderForUser tenant userAaid reminderId
         case deletedReminders of
            1 -> respondNoContent
            0 -> respondNotFound
            _ -> do
               logErrorS $ "Deleted " ++ show deletedReminders ++ " reminders with a single delete request. Primary key invalid on: " ++ show reminderId
               respondInternalServer
      Nothing -> respondWithError badRequest "A reminderId is required to see which reminder should be deleted."

addReminderHandler :: AC.TenantWithUser -> AppHandler ()
addReminderHandler connectTenant = do
  request <- SC.readRequestBody size10KB
  let errorOrReminder = eitherDecode request :: Either String ReminderRequest
  either (respondWithError badRequest) (addReminder connectTenant) errorOrReminder

-- TODO Just use the aaid we get from the JWT here
addReminder :: AC.TenantWithUser -> ReminderRequest -> AppHandler ()
addReminder (_, Nothing) _ = respondWithError unauthorised "You need to be logged in so that you can create a reminder. That way the reminder is against your account."
addReminder (tenant, Just userAaid) reminderRequest = writeError . runExceptT $ do
   when (userAaid /= requestAaid) $ throwE userMismatchError
   canSeeIssue <- withExceptT cantCheckPerms . ExceptT $ HU.canViewIssue tenant userAaid (AC.issueId . prqIssue $ reminderRequest)
   unless canSeeIssue $ throwE cantViewIssueError
   ExceptT (m2e insertFailedError <$> addReminderFromRequest reminderRequest tenant requestAaid)
  where
    requestAaid = prqUserAaid reminderRequest
    -- Error strings
    userMismatchError = (badRequest, "Given the details for user " ++ show requestAaid ++ " however Atlassian Connect thinks you are " ++ show userAaid)
    cantCheckPerms per = (internalServer, "Could not work out if you could view this issue: " ++ (T.unpack . AC.perMessage $ per))
    cantViewIssueError = (forbidden, "You can't view this issue so you cannot create a reminder for this issue")
    insertFailedError = (internalServer, "Failed to insert new reminder: " ++ show userAaid)

addReminderFromRequest :: ReminderRequest -> AC.Tenant -> P.UserAaid -> AppHandler (Maybe Integer)
addReminderFromRequest reminderRequest tenant userAaid =
  P.addReminder date' tenantId' iDetails userAaid message'
  where
    date' = prqReminderDate reminderRequest
    tenantId' = AC.tenantId tenant
    iDetails = prqIssue reminderRequest
    message' = prqMessage reminderRequest
