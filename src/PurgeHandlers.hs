module PurgeHandlers
   ( handlePurgeRequest
   ) where

import           Application
import           Data.Time.Clock (addUTCTime)
import           Data.TimeUnitUTC
import           Persistence.Tenant
import           Persistence.PostgreSQL
import qualified RemindMeConfiguration as RC
import qualified Snap.Core as SC
import           SnapHelpers

handlePurgeRequest :: AppHandler ()
handlePurgeRequest = handleMethods
   [ (SC.POST, purgeUninstalledTenants)
   ]

purgeUninstalledTenants :: AppHandler ()
purgeUninstalledTenants = getKeyAndConfirm RC.rmPurgeKey $ do
   currentTime <- getTimestampOrCurrentTime
   rmConf <- RC.getRMConf
   -- Calculate the date at which we will purge everything that has been uninstalled before it
   let purgeTime = addUTCTime (negate . timeUnitToDiffTime . RC.rmPurgeDuration $ rmConf) currentTime
   -- Get all of the tenants that can be purge
   withConnection $ \conn -> do
      -- TODO: Log the hostnames of all the tenants that we are about to purge...maybe even save that
      -- data
      markPurgedTenants purgeTime conn
      -- Delete all of the tenants that have been uninstalled for that long. (Their respective
      -- reminders should be destroyed at the same time).
      purgeTenants purgeTime conn
      return ()
