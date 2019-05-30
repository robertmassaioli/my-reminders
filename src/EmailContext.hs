module EmailContext
    ( EmailContext(..)
    ) where

import qualified AppConfig             as CONF
import qualified Data.Text             as T
import qualified Snap.AtlassianConnect as AC

data EmailContext = EmailContext
   { ecConnectConf        :: AC.Connect
   , ecPlainEmailTemplate :: T.Text
   , ecHtmlEmailTemplate  :: T.Text
   }

