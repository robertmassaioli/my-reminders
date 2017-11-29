module MicrosZone
    ( Zone(..)
    , zoneFromString
    , fromEnv
    , fromEnvDefaultDev
    , modifyDescriptorUsingZone
    ) where

import           Control.Monad           (join)
import qualified Data.EnvironmentHelpers as DE
import           Data.Maybe              (fromMaybe)
import qualified Data.Text               as T
import qualified Data.Connect.Descriptor as D

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
zoneFromString "dev.public.atl-paas.net" = Just Dev
zoneFromString "staging.public.atl-paas.net" = Just Dog
zoneFromString "prod.public.atl-paas.net" = Just Prod
zoneFromString _        = Nothing

fromEnv :: IO (Maybe Zone)
fromEnv = do
   envStr <- DE.getEnv "ZONE"
   return . join $ fmap zoneFromString envStr

fromEnvDefaultDev :: IO Zone
fromEnvDefaultDev = fmap (fromMaybe Dev) fromEnv

modifyDescriptorUsingZone :: Maybe Zone -> D.Plugin -> D.Plugin
modifyDescriptorUsingZone potentialZone descriptor = descriptor
    { D.pluginName = case D.pluginName descriptor of
        Nothing -> Nothing
        (Just (D.Name n)) -> Just . D.Name $ n `T.append` nameKeyAppend potentialZone
    , D.pluginKey = case D.pluginKey descriptor of (D.PluginKey k) -> D.PluginKey $ k `T.append` zoneKeyAppend potentialZone
    }

nameKeyAppend :: Maybe Zone -> T.Text
nameKeyAppend (Just Prod)   = T.empty
nameKeyAppend (Just zone)   = T.pack $ " (" ++ show zone ++ ")"
nameKeyAppend Nothing       = T.pack " (Local)"

zoneKeyAppend :: Maybe Zone -> T.Text
zoneKeyAppend (Just Prod)   = T.empty
zoneKeyAppend (Just Dog)    = T.pack ".dog"
zoneKeyAppend (Just Dev)    = T.pack ".dev"
zoneKeyAppend Nothing       = T.pack ".local"
