module AppHelpers
    ( logErrorS
    , getTimestampOrCurrentTime
    , integerPosixToUTCTime
    , getKeyAndConfirm
    ) where

import qualified AppConfig               as CONF
import           Application
import qualified Control.Applicative     as CA
import           Control.Monad.IO.Class  (liftIO)
import qualified Data.ByteString.Char8   as BSC
import           Data.Connect.Descriptor (Key (..))
import           Data.Time.Clock         (UTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX
import qualified Snap.Core               as SC
import qualified SnapHelpers as SH

logErrorS :: String -> AppHandler ()
logErrorS = SC.logError . BSC.pack

getTimestampOrCurrentTime :: AppHandler UTCTime
getTimestampOrCurrentTime =
  SC.getParam (BSC.pack "timestamp") >>= (\maybeRawTimestamp ->
    let maybeTimestamp = (integerPosixToUTCTime . read . BSC.unpack) CA.<$> maybeRawTimestamp :: Maybe UTCTime
    in maybe (liftIO getCurrentTime) return maybeTimestamp)

integerPosixToUTCTime :: Integer -> UTCTime
integerPosixToUTCTime = posixSecondsToUTCTime . fromIntegral

getKeyAndConfirm :: (CONF.AppConf -> Key BSC.ByteString CONF.AppConf) -> AppHandler () -> AppHandler ()
getKeyAndConfirm getKey success =
  SC.getParam (BSC.pack "key") >>=
    maybe
       (SH.respondWithError SH.forbidden "Speak friend and enter. However: http://i.imgur.com/fVDH5bN.gif")
       (\expireKey -> CONF.getAppConf >>= (\rmConf -> if getKey rmConf /= Key expireKey
           then SH.respondWithError SH.forbidden "You lack the required permissions."
           else success))

