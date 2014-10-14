module Data.EnvironmentHelpers
   ( getEnv
   , getEnvWithDefault
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
