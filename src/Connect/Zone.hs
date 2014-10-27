module Connect.Zone
   ( Zone(..)
   , zoneFromString
   , fromEnv
   , fromEnvDefaultDev
   ) where

import           Control.Monad           (join)
import qualified Data.EnvironmentHelpers as DE
import           Data.Maybe              (fromMaybe)

data Zone = Dev | Dog | Prod
   deriving(Eq, Show, Ord)

zoneFromString :: String -> Maybe Zone
zoneFromString "domain.dev.atlassian.io" = Just Dev
zoneFromString "application.dev.atlassian.io" = Just Dev
zoneFromString "platform.dev.atlassian.io" = Just Dev
zoneFromString "useast.staging.atlassian.io" = Just Dog
zoneFromString "uswest.staging.atlassian.io"  = Just Dog
zoneFromString "useast.atlassian.io" = Just Prod
zoneFromString "uswest.atlassian.io"  = Just Prod
zoneFromString _        = Nothing

fromEnv :: IO (Maybe Zone)
fromEnv = do
   envStr <- DE.getEnv "ZONE"
   return . join $ fmap zoneFromString envStr

fromEnvDefaultDev :: IO Zone
fromEnvDefaultDev = fmap (fromMaybe Dev) fromEnv
