{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module AaidSlurpHandlers ( handleAaidSlurp ) where

import           AesonHelpers
import           Application
import           Control.Monad                     (sequence)
import qualified Control.Exception      as E
import qualified Control.Exception.Lifted as EL
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Maybe                        (catMaybes)
import qualified Data.Text                         as T
import           GHC.Generics
import qualified Model.UserDetails                 as UD
import qualified Persistence.Reminder              as R
import qualified Snap.Core                         as SC
import qualified Snap.AtlassianConnect             as AC
import qualified Snap.AtlassianConnect.HostRequest as AC
import qualified SnapHelpers                       as SH

handleAaidSlurp :: AppHandler ()
handleAaidSlurp = SH.handleMethods
   [ (SC.PUT, handleAaidSlurpRun)
   ]

data SlurpRunResult = SlurpRunResult
  { srrErrors :: [T.Text]
  , ssrRemindersUpdated :: Int
  , ssrRemindersRemaining :: Int
  } deriving (Show, Generic)

instance ToJSON SlurpRunResult where
  toJSON = genericToJSON (baseOptions { fieldLabelModifier = stripFieldNamePrefix "ssr" })

-- TODO: For now we just catch everything but in the future we might choose to be more
-- selective...maybe.
exceptionFilter :: E.SomeException -> Maybe E.SomeException
exceptionFilter = Just

simpleCatch :: AppHandler a -> AppHandler (Either E.SomeException a)
simpleCatch = EL.tryJust exceptionFilter

-- Get all of the reminders with no aaid
-- For the first 10, get
handleAaidSlurpRun :: AppHandler ()
handleAaidSlurpRun = do
  reminders <- R.getRemindersMissingAaids
  -- TODO only take the first ten reminders
  let (toProcess, remainder) = splitAt 50 reminders
  responses <- sequence . fmap safeUpdateReminder $ toProcess
  let errors = getErrors responses
  let results = SlurpRunResult errors (length toProcess) (length remainder)
  SH.writeJson results
  SH.respondWith SH.ok

data UpdateReminderError
  = ProductErrorResponse AC.ProductErrorResponse
  | ExceptionResult E.SomeException

getErrors :: [Maybe UpdateReminderError] -> [T.Text]
getErrors = fmap toErrorMessage . catMaybes

toErrorMessage :: UpdateReminderError -> T.Text
toErrorMessasge (ProductErrorResponse e) = "Product Error Response: " `T.append` AC.perMessage e
toErrorMessage (ExceptionResult e) = "Exception: " `T.append` (T.pack . show $ e)

safeUpdateReminder :: (R.Reminder, AC.Tenant) -> AppHandler (Maybe UpdateReminderError)
safeUpdateReminder input = do
  result <- simpleCatch $ updateReminder input
  case result of
    Left e -> return . Just . ExceptionResult $ e
    Right (Left e) -> return . Just . ProductErrorResponse $ e
    _ -> return Nothing

updateReminder :: (R.Reminder, AC.Tenant) -> AppHandler (Either AC.ProductErrorResponse ())
updateReminder (reminder, tenant) = do
  potentialUserDetails <- UD.getUserDetails tenant (R.reminderUserKey reminder)
  case potentialUserDetails of
    Left error -> return . Left $ error
    Right userDetails -> do
      R.setReminderAccountId reminder (UD.accountId userDetails)
      return . Right $ ()
