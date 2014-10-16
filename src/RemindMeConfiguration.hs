{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module RemindMeConfiguration 
   ( RMConf(..)
   , HasRMConf(..)
   , initRMConf
   ) where

import           ConfigurationHelpers
import           Connect.Descriptor
import qualified Control.Monad.IO.Class as MI
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Configurator.Types as DCT
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Time.Units as DTU
import           Mail.Hailgun
import qualified Snap.Snaplet as SS

data RMConf = RMConf
   { rmExpireKey              :: Key BSC.ByteString RMConf
   , rmHailgunContext         :: HailgunContext
   , rmFromAddress            :: UnverifiedEmailAddress
   , rmMaxExpiryWindowMinutes :: DTU.Minute
   , rmPurgeKey               :: Key BSC.ByteString RMConf
   , rmPurgeRetention         :: DTU.Day
   }

class HasRMConf m where
   getRMConf :: m RMConf

instance DCT.Configured (Key BSC.ByteString a) where
  convert (DCT.String s) = Just (Key (encodeUtf8 s))
  convert _ = Nothing

initRMConf :: SS.SnapletInit b RMConf
initRMConf = SS.makeSnaplet "Remind Me Configuration" "Remind me configuration and state." (Just configDataDir) $
  MI.liftIO $ SS.loadAppConfig "remind-me.cfg" "resources" >>= loadRMConf

loadRMConf :: DCT.Config -> IO RMConf
loadRMConf config = do
   expiryKey <- require config "expiry-key" "Missing 'expiry-key': for triggering the reminders."
   mailgunDomain <- require config "mailgun-domain" "Missing 'mailgun-domain': required to send emails."
   mailgunApiKey <- require config "mailgun-api-key" "Missing 'mailgun-api-key': required to send emails."
   fromAddress <- require config "reminder-from-address" "Missing 'reminder-from-address': required for the inboxes of our customers to know who the reminder came from."
   maxExpiryWindowMinutes <- require config "expiry-window-max-minutes" "Missing 'expiry-window-max-minutes': it tracks how many minutes after expiry we should wait till we fail a healthcheck."
   purgeKey <- require config "purge-key" "Missing 'purge-key': for triggering customer data cleanups."
   purgeRetentionDays <- require config "purge-retention-days" "Missing 'purge-retention-days': the length of time that uninstalled customer data should remain before we delete it."
   return RMConf 
      { rmExpireKey = expiryKey
      , rmHailgunContext = HailgunContext
         { hailgunDomain = mailgunDomain
         , hailgunApiKey = mailgunApiKey
         }
      , rmFromAddress = fromAddress
      , rmMaxExpiryWindowMinutes = fromInteger (maxExpiryWindowMinutes :: Integer)
      , rmPurgeKey = purgeKey
      , rmPurgeRetention = fromInteger purgeRetentionDays
      }
