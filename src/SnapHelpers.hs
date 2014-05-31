module SnapHelpers where

import qualified Snap.Core as SC
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BSC

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
