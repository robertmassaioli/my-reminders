{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module RemindMeConfiguration 
   ( RMConf(..)
   , HasRMConf(..)
   , initRMConfOrExit
   ) where

import           ConfigurationHelpers
import           Control.Monad (when)
import           Connect.Descriptor
import qualified Control.Monad.IO.Class as MI
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Configurator.Types as DCT
import           Data.List (find)
import           Data.Maybe (fromMaybe)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Time.Units as DTU
import           Mail.Hailgun
import qualified Snap.Snaplet as SS
import qualified System.Environment as SE
import           System.Exit (ExitCode(..), exitWith)

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

initRMConfOrExit :: SS.SnapletInit b RMConf
initRMConfOrExit = SS.makeSnaplet "Remind Me Configuration" "Remind me configuration and state." (Just configDataDir) $
  MI.liftIO $ SS.loadAppConfig "remind-me.cfg" "resources" >>= loadRMConfOrExit

data Zone = Dev | Dog | Prod
   deriving(Eq, Show, Ord)

zoneFromString :: String -> Maybe Zone
zoneFromString "DEV"    = Just Dev
zoneFromString "DOG"    = Just Dog
zoneFromString "PROD"   = Just Prod
zoneFromString _        = Nothing

data EnvConf = EnvConf
   { ecExpireKey     :: Maybe String
   , ecPurgeKey      :: Maybe String
   , ecMailgunDomain :: Maybe String
   , ecMailgunApiKey :: Maybe String
   , ecZone          :: Zone
   } deriving (Eq, Show)

loadConfFromEnvironment :: IO EnvConf
loadConfFromEnvironment = do
   environment <- SE.getEnvironment
   let get = search environment
   return EnvConf
      { ecExpireKey     = get "EXPIRE_KEY"
      , ecPurgeKey      = get "PURGE_KEY"
      , ecMailgunDomain = get "MAILGUN_DOMAIN"
      , ecMailgunApiKey = get "MAILGUN_API_KEY"
      , ecZone          = fromMaybe Dev $ zoneFromString =<< get "ZONE"
      }

search :: [(String, String)] -> String -> Maybe String
search pairs key = fmap snd $ find ((==) key . fst) pairs

guardConfig :: EnvConf -> IO ()
guardConfig (EnvConf (Just _) (Just _) (Just _) (Just _) _) = return ()
guardConfig (EnvConf _        _        _        _        env) = 
   when (env `elem` [Dog, Prod]) $ do
      putStrLn $ "[Fatal Error] All of the environmental configuration is required in: " ++ show env
      exitWith (ExitFailure 10)

instance DCT.Configured (Key BSC.ByteString a) where
  convert (DCT.String s) = Just (Key (encodeUtf8 s))
  convert _ = Nothing

loadRMConfOrExit :: DCT.Config -> IO RMConf
loadRMConfOrExit config = do
   environmentConf <- loadConfFromEnvironment
   putStrLn "Loaded environment variable configuration:"
   print environmentConf
   guardConfig environmentConf

   expiryKey <- require config "expiry-key" "Missing 'expiry-key': for triggering the reminders."
   mailgunDomain <- require config "mailgun-domain" "Missing 'mailgun-domain': required to send emails."
   mailgunApiKey <- require config "mailgun-api-key" "Missing 'mailgun-api-key': required to send emails."
   fromAddress <- require config "reminder-from-address" "Missing 'reminder-from-address': required for the inboxes of our customers to know who the reminder came from."
   maxExpiryWindowMinutes <- require config "expiry-window-max-minutes" "Missing 'expiry-window-max-minutes': it tracks how many minutes after expiry we should wait till we fail a healthcheck."
   purgeKey <- require config "purge-key" "Missing 'purge-key': for triggering customer data cleanups."
   purgeRetentionDays <- require config "purge-retention-days" "Missing 'purge-retention-days': the length of time that uninstalled customer data should remain before we delete it."

   let fromConf = envOrDefault environmentConf
   return RMConf 
      { rmExpireKey = fromConf (fmap packInKey . ecExpireKey) expiryKey
      , rmHailgunContext = HailgunContext
         { hailgunDomain = fromConf ecMailgunDomain mailgunDomain
         , hailgunApiKey = fromConf ecMailgunApiKey mailgunApiKey
         }
      , rmFromAddress = fromAddress
      , rmMaxExpiryWindowMinutes = fromInteger (maxExpiryWindowMinutes :: Integer)
      , rmPurgeKey = fromConf (fmap packInKey . ecPurgeKey) purgeKey
      , rmPurgeRetention = fromInteger purgeRetentionDays
      }

packInKey :: String -> Key BSC.ByteString a
packInKey = Key . BSC.pack

envOrDefault :: EnvConf -> (EnvConf -> Maybe a) -> a -> a
envOrDefault env f def = fromMaybe def $ f env
