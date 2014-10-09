{-# LANGUAGE OverloadedStrings #-}

module RemindMeConfiguration 
   ( RMConf(..)
   , HasRMConf(..)
   , initRMConf
   ) where

import           ConfigurationHelpers
import qualified Control.Monad.IO.Class as MI
import qualified Data.Configurator.Types as DCT
import qualified Data.Time.Units as DTU
import           Mail.Hailgun
import qualified Snap.Snaplet as SS

data RMConf = RMConf
   { rmExpireKey              :: String
   , rmHailgunContext         :: HailgunContext
   , rmFromAddress            :: UnverifiedEmailAddress
   , rmMaxExpiryWindowMinutes :: DTU.Minute
   , rmPurgeKey               :: String
   , rmPurgeDuration          :: DTU.Day
   }

class HasRMConf m where
   getRMConf :: m RMConf

initRMConf :: SS.SnapletInit b RMConf
initRMConf = SS.makeSnaplet "Remind Me Configuration" "Remind me configuration and state." (Just configDataDir) $
  MI.liftIO $ SS.loadAppConfig "remind-me.cfg" "resources" >>= loadRMConf

loadRMConf :: DCT.Config -> IO RMConf
loadRMConf config = do
   expiryKey <- require config "expiry-key" "Missing an expiry key for triggering the reminders."
   mailgunDomain <- require config "mailgun-domain" "Missing Mailgun domain required to send emails."
   mailgunApiKey <- require config "mailgun-api-key" "Missing Mailgun api key required to send emails."
   fromAddress <- require config "reminder-from-address" "Missing a from address for the reminder. Required for the inboxes of our customers."
   maxExpiryWindowMinutes <- require config "expiry-window-max-minutes" "The Expiry Window Max Minutes is required; it tracks how many minutes after expiry we should wait till we fail a healthcheck."
   purgeKey <- require config "purge-key" "Missing a purge key for triggering customer data cleanups."
   purgeDuration <- require config "purge-duration" "Missing the length of time that uninstalled customer data should remain before we delete it."
   return RMConf 
      { rmExpireKey = expiryKey
      , rmHailgunContext = HailgunContext
         { hailgunDomain = mailgunDomain
         , hailgunApiKey = mailgunApiKey
         }
      , rmFromAddress = fromAddress
      , rmMaxExpiryWindowMinutes = fromInteger (maxExpiryWindowMinutes :: Integer)
      , rmPurgeKey = purgeKey
      , rmPurgeDuration = fromInteger purgeDuration
      }
