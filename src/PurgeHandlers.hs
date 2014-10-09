module PurgeHandlers
   ( handlePurgeRequest
   ) where

import           Application
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
   return ()
