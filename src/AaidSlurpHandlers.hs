{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module AaidSlurpHandlers ( handleAaidSlurp ) where

import           AesonHelpers
import           Application
import           Control.Monad                     (sequence)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Either                       (lefts)
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

-- Get all of the reminders with no aaid
-- For the first 10, get
handleAaidSlurpRun :: AppHandler ()
handleAaidSlurpRun = do
  reminders <- R.getRemindersMissingAaids
  -- TODO only take the first ten reminders
  let (toProcess, remainder) = splitAt 50 reminders
  responses <- sequence . fmap updateReminder $ toProcess
  let errors = fmap (AC.perMessage) . lefts $ responses
  let results = SlurpRunResult errors (length toProcess) (length remainder)
  SH.writeJson results
  SH.respondWith SH.ok

updateReminder :: (R.Reminder, AC.Tenant) -> AppHandler (Either AC.ProductErrorResponse ())
updateReminder (reminder, tenant) = do
  potentialUserDetails <- UD.getUserDetails tenant (R.reminderUserKey reminder)
  case potentialUserDetails of
    Left error -> return . Left $ error
    Right userDetails -> do
      R.setReminderAccountId reminder (UD.accountId userDetails)
      return . Right $ ()
