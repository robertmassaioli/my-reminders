module EmailContext
    ( EmailContext(..)
    ) where

import qualified AppConfig             as CONF
import qualified Data.Text             as T
import           Mail.Hailgun
import qualified Snap.AtlassianConnect as AC

data EmailContext = EmailContext
   { ecConnectConf        :: AC.Connect
   , ecAppConf            :: CONF.AppConf
   , ecPlainEmailTemplate :: T.Text
   , ecHtmlEmailTemplate  :: T.Text
   , ecAttachments        :: [Attachment]
   }

