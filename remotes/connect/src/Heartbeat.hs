module Heartbeat (heartbeatRequest) where

import           Application
import qualified Snap.Core as SC
import           SnapHelpers (handleMethods, respondWith, ok)

heartbeatRequest :: AppHandler ()
heartbeatRequest = handleMethods
   [ ( SC.GET, respondWith ok)
   ]
