{-# LANGUAGE DeriveGeneric #-}
module SnapHelpers where

import           Data.Aeson
import           GHC.Generics

import qualified Control.Applicative     as CA
import qualified Data.ByteString.Char8   as BSC
import qualified Data.Text               as T
import qualified Snap.Core               as SC

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