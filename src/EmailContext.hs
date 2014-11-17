module EmailContext
    ( EmailContext(..)
    ) where

import qualified AppConfig       as CONF
import           Connect.Connect
import qualified Data.Text       as T
import           Mail.Hailgun

data EmailContext = EmailContext
   { ecConnectConf        :: Connect
   , ecAppConf            :: CONF.AppConf
   , ecPlainEmailTemplate :: T.Text
   , ecHtmlEmailTemplate  :: T.Text
   , ecAttachments        :: [Attachment]
   }

