{-# LANGUAGE OverloadedStrings #-}

module RemindMeConfiguration 
   ( RMConf(..)
   , HasRMConf(..)
   , initRMConf
   ) where

import           ConfigurationHelpers
import qualified Control.Monad.IO.Class as MI
import qualified Data.Configurator.Types as DCT
import qualified Snap.Snaplet as SS

data RMConf = RMConf
   { rmExpireKey :: String
   }

class HasRMConf m where
   getRMConf :: m RMConf

initRMConf :: SS.SnapletInit b RMConf
initRMConf = SS.makeSnaplet "Remind Me Configuration" "Remind me configuration and state." (Just configDataDir) $
  MI.liftIO $ SS.loadAppConfig "remind-me.cfg" "resources" >>= loadRMConf

loadRMConf :: DCT.Config -> IO RMConf
loadRMConf config = do
   expiryKey <- require config "expiry_key" "Missing an expiry key for triggering the reminders."
   return RMConf 
      { rmExpireKey = expiryKey
      }
