{-# LANGUAGE OverloadedStrings #-}

module RemindMeConfiguration 
   ( RMConf(..)
   , HasRMConf(..)
   , initRMConf
   ) where

import           ConfigurationHelpers
import           Control.Monad (when)
import qualified Control.Monad.IO.Class as MI
import           Connect.Zone
import qualified Data.Configurator.Types as DCT
import           Data.List (find)
import           Data.Maybe (fromMaybe)
import qualified Data.Time.Units as DTU
import           Mail.Hailgun
import qualified Snap.Snaplet as SS
import qualified System.Environment as SE
import           System.Exit (ExitCode(..), exitWith)

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

loadRMConf :: DCT.Config -> IO RMConf
loadRMConf config = do
   environmentConf <- loadConfFromEnvironment
   putStrLn "Loaded environment variable configuartion:"
   print environmentConf
   guardConfig environmentConf

   expiryKey <- require config "expiry-key" "Missing an expiry key for triggering the reminders."
   mailgunDomain <- require config "mailgun-domain" "Missing Mailgun domain required to send emails."
   mailgunApiKey <- require config "mailgun-api-key" "Missing Mailgun api key required to send emails."
   fromAddress <- require config "reminder-from-address" "Missing a from address for the reminder. Required for the inboxes of our customers."
   maxExpiryWindowMinutes <- require config "expiry-window-max-minutes" "The Expiry Window Max Minutes is required; it tracks how many minutes after expiry we should wait till we fail a healthcheck."
   purgeKey <- require config "purge-key" "Missing a purge key for triggering customer data cleanups."
   purgeDuration <- require config "purge-duration-days" "Missing the length of time that uninstalled customer data should remain before we delete it."

   let fromConf = envOrDefault environmentConf
   return RMConf 
      { rmExpireKey = fromConf ecExpireKey expiryKey
      , rmHailgunContext = HailgunContext
         { hailgunDomain = fromConf ecMailgunDomain mailgunDomain
         , hailgunApiKey = fromConf ecMailgunApiKey mailgunApiKey
         }
      , rmFromAddress = fromAddress
      , rmMaxExpiryWindowMinutes = fromInteger (maxExpiryWindowMinutes :: Integer)
      , rmPurgeKey = fromConf ecPurgeKey purgeKey
      , rmPurgeDuration = fromInteger purgeDuration
      }

envOrDefault :: EnvConf -> (EnvConf -> Maybe a) -> a -> a
envOrDefault env f def = fromMaybe def $ f env
