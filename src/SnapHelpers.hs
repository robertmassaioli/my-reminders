{-# LANGUAGE DeriveGeneric #-}
module SnapHelpers where

import qualified Control.Applicative    as CA
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import qualified Data.ByteString.Char8  as BSC
import           Data.Int               (Int64)
import qualified Data.Text              as T
import           Data.Time.Clock        (UTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX  (posixSecondsToUTCTime)
import           GHC.Generics
import qualified Snap.Core              as SC
import qualified Snap.Snaplet           as SS

respondWith :: SC.MonadSnap m => Int -> m ()
respondWith = SC.modifyResponse . SC.setResponseCode

noContent, temporaryRedirect, badRequest, unauthorised, forbidden, notFound, internalServer, ok, serviceUnavaliable :: Int
ok                 = 200
noContent          = 204
temporaryRedirect  = 302
badRequest         = 400
unauthorised       = 401
forbidden          = 403
notFound           = 404
internalServer     = 500
serviceUnavaliable = 503

respondBadRequest, respondNotFound, respondInternalServer, respondNoContent :: SC.MonadSnap m => m ()
respondBadRequest       = respondWith badRequest
respondNotFound         = respondWith notFound
respondInternalServer   = respondWith internalServer
respondNoContent        = respondWith noContent

textToByteString :: T.Text -> BSC.ByteString
textToByteString = BSC.pack . T.unpack

byteStringToText :: BSC.ByteString -> T.Text
byteStringToText = T.pack . BSC.unpack

handleMethods :: SC.MonadSnap s => [(SC.Method, s z)] -> s z
handleMethods = foldl (CA.<|>) CA.empty . fmap (uncurry SC.method)

writeJson :: (SC.MonadSnap m, ToJSON a) => a -> m ()
writeJson a = do
  SC.modifyResponse . SC.setContentType . BSC.pack $ "application/json"
  SC.writeLBS $ encode a

data ErrorResponse = ErrorResponse
  { errorMessages :: [String]
  } deriving (Show, Generic)

instance ToJSON ErrorResponse

respondWithError :: SC.MonadSnap m => Int -> String -> m ()
respondWithError errorCode response = respondWithErrors errorCode [response]

respondWithErrors :: SC.MonadSnap m => Int -> [String] -> m ()
respondWithErrors errorCode responses = do
  writeJson errorResponse
  respondWith errorCode
  where
    errorResponse = ErrorResponse responses

respondPlainWithError :: SC.MonadSnap m => Int -> String -> m ()
respondPlainWithError errorCode response = do
  SC.writeBS . BSC.pack $ response
  respondWith errorCode

logErrorS :: String -> SS.Handler a b ()
logErrorS = SC.logError . BSC.pack

getTimestampOrCurrentTime :: SS.Handler a b UTCTime
getTimestampOrCurrentTime =
  SC.getParam (BSC.pack "timestamp") >>= (\maybeRawTimestamp ->
    let maybeTimestamp = (integerPosixToUTCTime . read . BSC.unpack) CA.<$> maybeRawTimestamp :: Maybe UTCTime
    in maybe (liftIO getCurrentTime) return maybeTimestamp)

integerPosixToUTCTime :: Integer -> UTCTime
integerPosixToUTCTime = posixSecondsToUTCTime . fromIntegral

size1KB, size10KB, size500KB, size1MB, size5MB, size10MB :: Int64
size1KB = 1024
size10KB = size1KB * 10
size500KB = size1KB * 500
size1MB = size10KB * 1024
size5MB = size1MB * 5
size10MB = size1MB * 10
