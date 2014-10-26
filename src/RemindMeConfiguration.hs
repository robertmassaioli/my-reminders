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
import           Connect.Zone
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Configurator.Types as DCT
import qualified Data.EnvironmentHelpers as DE
import           Data.List (find)
import           Data.Maybe (fromMaybe)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Time.Units as DTU
import           Mail.Hailgun
import           Network.HTTP.Client (Proxy(..))
import qualified Snap.Snaplet as SS
import qualified System.Environment as SE
import           System.Exit (ExitCode(..), exitWith)
import           Text.PrettyPrint.Boxes

data RMConf = RMConf
   { rmExpireKey              :: Key BSC.ByteString RMConf
   , rmHailgunContext         :: HailgunContext
   , rmFromAddress            :: UnverifiedEmailAddress
   , rmMaxExpiryWindowMinutes :: DTU.Minute
   , rmPurgeKey               :: Key BSC.ByteString RMConf
   , rmPurgeRetention         :: DTU.Day
   , rmHttpProxy              :: Maybe Proxy
   , rmHttpSecureProxy        :: Maybe Proxy
   }

class HasRMConf m where
   getRMConf :: m RMConf

initRMConfOrExit :: SS.SnapletInit b RMConf
initRMConfOrExit = SS.makeSnaplet "Remind Me Configuration" "Remind me configuration and state." (Just configDataDir) $
  MI.liftIO $ SS.loadAppConfig "remind-me.cfg" "resources" >>= loadRMConfOrExit

data EnvConf = EnvConf
   { ecExpireKey        :: Maybe String
   , ecPurgeKey         :: Maybe String
   , ecMailgunDomain    :: Maybe String
   , ecMailgunApiKey    :: Maybe String
   , ecHttpProxy        :: Maybe String
   , ecHttpSecureProxy  :: Maybe String
   , ecZone             :: Zone
   } deriving (Eq, Show)

loadConfFromEnvironment :: IO EnvConf
loadConfFromEnvironment = do
   environment <- SE.getEnvironment
   let get = search environment
   return EnvConf
      { ecExpireKey        = get "EXPIRE_KEY"
      , ecPurgeKey         = get "PURGE_KEY"
      , ecMailgunDomain    = get "MAILGUN_DOMAIN"
      , ecMailgunApiKey    = get "MAILGUN_API_KEY"
      , ecHttpProxy        = get "http_proxy"
      , ecHttpSecureProxy  = get "https_proxy"
      , ecZone             = fromMaybe Dev $ zoneFromString =<< get "ZONE"
      }

search :: [(String, String)] -> String -> Maybe String
search pairs key = fmap snd $ find ((==) key . fst) pairs

guardConfig :: EnvConf -> IO ()
guardConfig (EnvConf (Just _) (Just _) (Just _) (Just _) _ _ _) = return ()
guardConfig (EnvConf _        _        _        _        _ _ env) = 
   when (env `elem` [Dog, Prod]) $ do
      putStrLn $ "[Fatal Error] All of the environmental configuration is required in: " ++ show env
      exitWith (ExitFailure 10)

instance DCT.Configured (Key BSC.ByteString a) where
  convert (DCT.String s) = Just (Key (encodeUtf8 s))
  convert _ = Nothing

loadRMConfOrExit :: DCT.Config -> IO RMConf
loadRMConfOrExit config = do
   environmentConf <- loadConfFromEnvironment
   printEnvironmentConf environmentConf
   guardConfig environmentConf

   expiryKey <- require config "expiry-key" "Missing 'expiry-key': for triggering the reminders."
   mailgunDomain <- require config "mailgun-domain" "Missing 'mailgun-domain': required to send emails."
   mailgunApiKey <- require config "mailgun-api-key" "Missing 'mailgun-api-key': required to send emails."
   fromAddress <- require config "reminder-from-address" "Missing 'reminder-from-address': required for the inboxes of our customers to know who the reminder came from."
   maxExpiryWindowMinutes <- require config "expiry-window-max-minutes" "Missing 'expiry-window-max-minutes': it tracks how many minutes after expiry we should wait till we fail a healthcheck."
   purgeKey <- require config "purge-key" "Missing 'purge-key': for triggering customer data cleanups."
   purgeRetentionDays <- require config "purge-retention-days" "Missing 'purge-retention-days': the length of time that uninstalled customer data should remain before we delete it."

   let httpProxy = parseProxy standardHttpPort (ecHttpProxy environmentConf)
   let httpSecureProxy = parseProxy standardHttpSecurePort (ecHttpSecureProxy environmentConf)
   let fromConf = envOrDefault environmentConf
   return RMConf 
      { rmExpireKey = fromConf (fmap packInKey . ecExpireKey) expiryKey
      , rmHailgunContext = HailgunContext
         { hailgunDomain = fromConf ecMailgunDomain mailgunDomain
         , hailgunApiKey = fromConf ecMailgunApiKey mailgunApiKey
         , hailgunProxy  = httpSecureProxy
         }
      , rmFromAddress = fromAddress
      , rmMaxExpiryWindowMinutes = fromInteger (maxExpiryWindowMinutes :: Integer)
      , rmPurgeKey = fromConf (fmap packInKey . ecPurgeKey) purgeKey
      , rmPurgeRetention = fromInteger purgeRetentionDays
      , rmHttpProxy = httpProxy
      , rmHttpSecureProxy = httpSecureProxy
      }

packInKey :: String -> Key BSC.ByteString a
packInKey = Key . BSC.pack

envOrDefault :: EnvConf -> (EnvConf -> Maybe a) -> a -> a
envOrDefault env f def = fromMaybe def $ f env

boxEnvironmentConf :: EnvConf -> Box
boxEnvironmentConf c = 
   text "## Environmental Configuration" //
   (vcat left
      [ text " - Expire Key:"
      , text " - Purge Key:"
      , text " - Mailgun Domain:"
      , text " - Mailgun Api Key:"
      , text " - HTTP Proxy:"
      , text " - HTTP Secure Proxy:"
      ]
   <+> vcat left
      [ text . DE.showMaybe . ecExpireKey $ c
      , text . DE.showMaybe . ecPurgeKey $ c
      , text . DE.showMaybe . ecMailgunDomain $ c
      , text . DE.showMaybe . ecMailgunApiKey $ c
      , text . DE.showMaybe . ecHttpProxy $ c
      , text . DE.showMaybe . ecHttpSecureProxy $ c
      ]
   )

printEnvironmentConf :: EnvConf -> IO ()
printEnvironmentConf = printBox . boxEnvironmentConf

type HttpPort = Int

standardHttpPort :: HttpPort
standardHttpPort = 80

standardHttpSecurePort :: HttpPort
standardHttpSecurePort = 443

parseProxy :: HttpPort -> Maybe String -> Maybe Proxy
parseProxy defaultPort potentialRawProxy = do
   rawProxy <- potentialRawProxy
   let (host, portAndColon) = span (/= ':') rawProxy
   let port = if null portAndColon then [] else tail portAndColon
   return $ Proxy (BSC.pack host) (if null port then defaultPort else read port)
