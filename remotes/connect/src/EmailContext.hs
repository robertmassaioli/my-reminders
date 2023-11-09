module EmailContext
    ( EmailContext(..)
    ) where

import qualified Snap.AtlassianConnect as AC
import qualified Text.Mustache         as M

data EmailContext = EmailContext
   { ecConnectConf        :: AC.Connect
   , ecPlainEmailTemplate :: M.Template
   , ecHtmlEmailTemplate  :: M.Template
   }

