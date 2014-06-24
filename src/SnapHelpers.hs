module SnapHelpers where

import Data.Aeson

import qualified Control.Applicative as CA
import qualified Snap.Core as SC
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BSC

import Application

respondWith :: SC.MonadSnap m => Int -> m ()
respondWith = SC.modifyResponse . SC.setResponseCode

respondBadRequest       :: SC.MonadSnap m => m ()
respondInternalServer   :: SC.MonadSnap m => m ()
respondNoContent        :: SC.MonadSnap m => m ()
respondBadRequest       = respondWith 400
respondInternalServer   = respondWith 500
respondNoContent        = respondWith 204

textToByteString :: T.Text -> BSC.ByteString
textToByteString = BSC.pack . T.unpack

byteStringToText :: BSC.ByteString -> T.Text
byteStringToText = T.pack . BSC.unpack

handleMethods :: SC.MonadSnap s => [(SC.Method, s z)] -> s z
handleMethods = foldl (CA.<|>) CA.empty . fmap (uncurry SC.method)

writeJson :: (SC.MonadSnap m, ToJSON a) => a -> m ()
writeJson a = do
  SC.modifyResponse . SC.setContentType . BSC.pack $"application/json"
  SC.writeLBS $ encode a

logErrorS :: String -> AppHandler ()
logErrorS = SC.logError . BSC.pack
