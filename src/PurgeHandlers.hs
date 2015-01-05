module PurgeHandlers
   ( handlePurgeRequest
   ) where

import qualified AppConfig              as CONF
import           AppHelpers
import           Application
import           Data.Time.Clock        (addUTCTime)
import           Data.TimeUnitUTC
import           Persistence.PostgreSQL
import           Persistence.Tenant
import qualified Snap.Core              as SC
import           SnapHelpers

handlePurgeRequest :: AppHandler ()
handlePurgeRequest = handleMethods
   [ (SC.POST, purgeUninstalledTenants)
   ]

purgeUninstalledTenants :: AppHandler ()
purgeUninstalledTenants = getKeyAndConfirm CONF.rmPurgeKey $ do
   currentTime <- getTimestampOrCurrentTime
   rmConf <- CONF.getAppConf
   -- Calculate the date for which any tenant uninstalled prior should be purged
   let purgeTime = addUTCTime (negate . timeUnitToDiffTime . CONF.rmPurgeRetention $ rmConf) currentTime
   -- Get all of the tenants that can be purge
   withConnection $ \conn -> do
      -- Save the hostnames of all the tenants that we are about to purge
      markPurgedTenants purgeTime conn
      -- Delete all of the tenants that have been uninstalled for that long. (Their respective
      -- reminders should be destroyed at the same time).
      purgeTenants purgeTime conn
      return ()
