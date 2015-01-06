module AppHelpers
    ( getKeyAndConfirm
    ) where

import qualified AppConfig               as CONF
import           Application
import qualified Data.ByteString.Char8   as BSC
import           Data.Connect.Descriptor (Key (..))
import qualified Snap.Core               as SC
import qualified SnapHelpers             as SH

getKeyAndConfirm
    :: (CONF.AppConf -> Key BSC.ByteString CONF.AppConf)
    -> AppHandler ()
    -> AppHandler ()
getKeyAndConfirm getKey success =
  SC.getParam (BSC.pack "key") >>=
    maybe
       (SH.respondWithError SH.forbidden "Speak friend and enter. However: http://i.imgur.com/fVDH5bN.gif")
       (\expireKey -> CONF.getAppConf >>= (\rmConf -> if getKey rmConf /= Key expireKey
           then SH.respondWithError SH.forbidden "You lack the required permissions."
           else success))

