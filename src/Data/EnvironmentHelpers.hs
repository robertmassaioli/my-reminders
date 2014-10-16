module Data.EnvironmentHelpers
   ( getEnv
   , getEnvWithDefault
   , showMaybe
   ) where

import           Data.List (find)
import           Data.Maybe (fromMaybe)
import qualified System.Environment as E

getEnv :: String -> IO (Maybe String)
getEnv key = do
   env <- E.getEnvironment
   return . fmap snd . find ((==) key . fst) $ env

getEnvWithDefault :: String -> String -> IO String
getEnvWithDefault def = fmap (fromMaybe def) . getEnv

showMaybe :: Show a => Maybe a -> String
showMaybe Nothing  = "[Not Available]"
showMaybe (Just x) = show x
