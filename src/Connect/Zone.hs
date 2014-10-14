module Connect.Zone 
   ( Zone(..)
   , zoneFromString
   , fromEnv
   , fromEnvDefaultDev
   ) where

import           Control.Monad (join)
import           Data.Maybe (fromMaybe)
import qualified Data.EnvironmentHelpers as DE

data Zone = Dev | Dog | Prod
   deriving(Eq, Show, Ord)

zoneFromString :: String -> Maybe Zone
zoneFromString "DEV"    = Just Dev
zoneFromString "DOG"    = Just Dog
zoneFromString "PROD"   = Just Prod
zoneFromString _        = Nothing

fromEnv :: IO (Maybe Zone)
fromEnv = do
   envStr <- DE.getEnv "ZONE"
   return . join $ fmap zoneFromString envStr

fromEnvDefaultDev :: IO Zone
fromEnvDefaultDev = fmap (fromMaybe Dev) fromEnv
