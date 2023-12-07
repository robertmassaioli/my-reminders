module ConfigurationHelpers
   ( require
   ) where

import qualified Data.Configurator        as DC
import qualified Data.Configurator.Types  as DCT

require :: DCT.Configured a => DCT.Config -> DCT.Name -> String -> IO a
require config name errorMessage = do
  potentialValue <- DC.lookup config name
  case potentialValue of
    Nothing -> fail errorMessage
    Just x -> return x
