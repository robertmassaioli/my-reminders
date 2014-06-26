{-# LANGUAGE DeriveGeneric #-}

module SnapHelpers where

import Data.Aeson
import GHC.Generics

import qualified Control.Applicative as CA
import qualified Snap.Core as SC
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BSC

import Application

respondWith :: SC.MonadSnap m => Int -> m ()
respondWith = SC.modifyResponse . SC.setResponseCode

noContent :: Int      
badRequest :: Int
unauthorised :: Int
forbidden :: Int
notFound :: Int
internalServer :: Int
noContent      = 204
badRequest     = 400
unauthorised   = 401
forbidden      = 403
notFound       = 404
internalServer = 500

respondBadRequest       :: SC.MonadSnap m => m ()
respondNotFound         :: SC.MonadSnap m => m ()
respondInternalServer   :: SC.MonadSnap m => m ()
respondNoContent        :: SC.MonadSnap m => m ()
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
  SC.modifyResponse . SC.setContentType . BSC.pack $"application/json"
  SC.writeLBS $ encode a

logErrorS :: String -> AppHandler ()
logErrorS = SC.logError . BSC.pack

data ErrorResponse = ErrorResponse
   { errorMessage :: String
   } deriving (Show, Generic)

instance ToJSON ErrorResponse

respondWithError :: Int -> String -> AppHandler ()
respondWithError errorCode response = do
   SC.writeLBS . encode $ errorResponse
   respondWith errorCode
   where
      errorResponse = ErrorResponse response
