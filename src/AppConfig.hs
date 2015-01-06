{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module AppConfig
   ( AppConf(..)
   , HasAppConf(..)
   , initAppConfOrExit
   ) where

import           ConfigurationHelpers
import           Control.Applicative        (pure, (<$>), (<*>))
import           Control.Monad              (join, when)
import qualified Control.Monad.IO.Class     as MI
import qualified Data.ByteString.Char8      as BSC
import qualified Data.Configurator.Types    as DCT
import           Data.ConfiguratorTimeUnits ()
import qualified Data.Connect.Descriptor    as D
import qualified Data.EnvironmentHelpers    as DE
import           Data.List                  (find)
import           Data.Maybe                 (fromMaybe, isJust)
import           Data.Text.Encoding         (encodeUtf8)
import qualified Data.Time.Units            as DTU
import           Mail.Hailgun
import qualified MicrosZone                 as MZ
import           Network.HTTP.Client        (Proxy (..))
import qualified Network.URI                as NU
import qualified Snap.Snaplet               as SS
import qualified System.Environment         as SE
import           System.Exit                (ExitCode (..), exitWith)
import           Text.PrettyPrint.Boxes

data AppConf = AppConf
   { rmExpireKey              :: D.Key BSC.ByteString AppConf
   , rmHailgunContext         :: HailgunContext
   , rmFromUser               :: String
   , rmMaxExpiryWindowMinutes :: DTU.Minute
   , rmPurgeKey               :: D.Key BSC.ByteString AppConf
   , rmPurgeRetention         :: DTU.Day
   , rmHttpProxy              :: Maybe Proxy
   , rmHttpSecureProxy        :: Maybe Proxy
   , rmMigrationKey           :: D.Key BSC.ByteString AppConf
   , rmStatisticsKey          :: D.Key BSC.ByteString AppConf
   }

class HasAppConf m where
   getAppConf :: m AppConf

initAppConfOrExit :: IO String -> SS.SnapletInit b AppConf
initAppConfOrExit configDataDir = SS.makeSnaplet "Applicaiton Configuration" "Application configuration and state." (Just configDataDir) $
  MI.liftIO $ SS.loadAppConfig "my-reminders.cfg" "resources" >>= loadAppConfOrExit

data EnvConf = EnvConf
   { ecExpireKey       :: Maybe String
   , ecPurgeKey        :: Maybe String
   , ecMigrationKey    :: Maybe String
   , ecStatisticsKey   :: Maybe String
   , ecMailgunDomain   :: Maybe String
   , ecMailgunApiKey   :: Maybe String
   , ecHttpProxy       :: Maybe String
   , ecHttpSecureProxy :: Maybe String
   , ecZone            :: MZ.Zone
   } deriving (Eq, Show)

loadConfFromEnvironment :: IO EnvConf
loadConfFromEnvironment = do
   environment <- SE.getEnvironment
   let get = search environment
   return EnvConf
      { ecExpireKey        = get "EXPIRE_KEY"
      , ecPurgeKey         = get "PURGE_KEY"
      , ecMigrationKey     = get "MIGRATION_KEY"
      , ecStatisticsKey    = get "STATISTICS_KEY"
      , ecMailgunDomain    = get "MAILGUN_DOMAIN"
      , ecMailgunApiKey    = get "MAILGUN_API_KEY"
      , ecHttpProxy        = get "http_proxy"
      , ecHttpSecureProxy  = get "https_proxy"
      , ecZone             = fromMaybe MZ.Dev $ MZ.zoneFromString =<< get "ZONE"
      }

search :: [(String, String)] -> String -> Maybe String
search pairs key = snd <$> find ((==) key . fst) pairs

guardConfig :: EnvConf -> IO ()
guardConfig ec = when (isDogOrProd ec && not allKeysPresent) $ do
   putStrLn $ "[Fatal Error] All of the environmental configuration is required in: " ++ (show . ecZone $ ec)
   exitWith (ExitFailure 10)
   where
      allKeysPresent = all isJust $ [ecExpireKey, ecPurgeKey, ecMigrationKey, ecMailgunDomain, ecMailgunApiKey] <*> pure ec

instance DCT.Configured (D.Key BSC.ByteString a) where
  convert (DCT.String s) = Just (D.Key (encodeUtf8 s))
  convert _ = Nothing

loadAppConfOrExit :: DCT.Config -> IO AppConf
loadAppConfOrExit config = do
   environmentConf <- loadConfFromEnvironment
   printEnvironmentConf environmentConf
   guardConfig environmentConf

   statisticsKey <- require config "statistics-key" "Missing 'statistics-key': for gettinig statistics data."
   migrationKey <- require config "migration-key" "Missing 'migration-key': for trigerring migrations."
   expiryKey <- require config "expiry-key" "Missing 'expiry-key': for triggering the reminders."
   mailgunDomain <- require config "mailgun-domain" "Missing 'mailgun-domain': required to send emails."
   mailgunApiKey <- require config "mailgun-api-key" "Missing 'mailgun-api-key': required to send emails."
   fromUser <- require config "reminder-from-user" "Missing 'reminder-from-user': required for the inboxes of our customers to know who the reminder came from."
   maxExpiryWindowMinutes <- require config "expiry-window-max-minutes" "Missing 'expiry-window-max-minutes': it tracks how many minutes after expiry we should wait till we fail a healthcheck."
   purgeKey <- require config "purge-key" "Missing 'purge-key': for triggering customer data cleanups."
   purgeRetentionDays <- require config "purge-retention-days" "Missing 'purge-retention-days': the length of time that uninstalled customer data should remain before we delete it."

   let httpProxy = parseProxy standardHttpPort (ecHttpProxy environmentConf)
   let httpSecureProxy = parseProxy standardHttpSecurePort (ecHttpSecureProxy environmentConf)
   let fromConf = envOrDefault environmentConf
   return AppConf
      { rmExpireKey = fromConf (fmap packInKey . ecExpireKey) expiryKey
      , rmHailgunContext = HailgunContext
         { hailgunDomain = fromConf ecMailgunDomain mailgunDomain
         , hailgunApiKey = fromConf ecMailgunApiKey mailgunApiKey
         , hailgunProxy  = httpSecureProxy
         }
      , rmFromUser = fromUser
      , rmMaxExpiryWindowMinutes = maxExpiryWindowMinutes
      , rmPurgeKey = fromConf (fmap packInKey . ecPurgeKey) purgeKey
      , rmPurgeRetention = fromInteger purgeRetentionDays
      , rmHttpProxy = httpProxy
      , rmHttpSecureProxy = httpSecureProxy
      , rmMigrationKey = fromConf (fmap packInKey . ecMigrationKey) migrationKey
      , rmStatisticsKey = fromConf (fmap packInKey . ecStatisticsKey) statisticsKey
      }

isDogOrProd :: EnvConf -> Bool
isDogOrProd ec = ecZone ec `elem` [MZ.Dog, MZ.Prod]

packInKey :: String -> D.Key BSC.ByteString a
packInKey = D.Key . BSC.pack

envOrDefault :: EnvConf -> (EnvConf -> Maybe a) -> a -> a
envOrDefault env f def = fromMaybe def $ f env

boxEnvironmentConf :: EnvConf -> Box
boxEnvironmentConf c =
   text "## Environmental Configuration" //
   (vcat left
      [ text " - Expire Key:"
      , text " - Purge Key:"
      , text " - Statistics Key:"
      , text " - Mailgun Domain:"
      , text " - Mailgun Api Key:"
      , text " - HTTP Proxy:"
      , text " - HTTP Secure Proxy:"
      ]
   <+> vcat left
      [ text . DE.showMaybe . ecExpireKey $ c
      , text . DE.showMaybe . ecPurgeKey $ c
      , text . DE.showMaybe . ecStatisticsKey $ c
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
   authority <- join $ fmap NU.uriAuthority $ NU.parseURI rawProxy
   let (host, portAndColon) = (NU.uriRegName authority, NU.uriPort authority)
   let port = if null portAndColon then [] else tail portAndColon
   return $ Proxy (BSC.pack host) (if null port then defaultPort else read port)
