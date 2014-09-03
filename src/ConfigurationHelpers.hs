module ConfigurationHelpers
   ( require
   , configDataDir
   ) where

import qualified Control.Monad as CM
import qualified Data.Configurator as DC
import qualified Data.Configurator.Types as DCT

import qualified Paths_ping_me_connect as PPMC

require :: DCT.Configured a => DCT.Config -> DCT.Name -> String -> IO a
require config name errorMessage = do
  potentialValue <- DC.lookup config name
  case potentialValue of
    Nothing -> fail errorMessage
    Just x -> return x

configDataDir :: IO String
configDataDir = CM.liftM (++ "/resources") PPMC.getDataDir
