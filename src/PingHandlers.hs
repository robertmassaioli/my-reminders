{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}

module PingHandlers(addPingHandler, executePingsHandler) where

import qualified Data.Text as T 
import qualified Data.ByteString.Char8 as BSC
import Snap.Core
import Snap.Snaplet
import Data.Aeson
import Data.Maybe
import Application
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Database.PostgreSQL.Simple
import Persistence.PostgreSQL
import qualified Persistence.Ping as P 
import GHC.Generics
import Network.URI
import Control.Monad.IO.Class

data PingRequest = PingRequest {
                               userID:: T.Text,
                               timeStamp :: Integer,
                               issueLink:: String,
                               message:: T.Text,
                               tenantId:: Integer --TODO: this needs to be based on something
                               }
                   deriving (Show,Generic)

instance FromJSON PingRequest
instance ToJSON PingRequest

--TODO: this needs to be authenticated
addPingHandler:: AppHandler ()
addPingHandler = do
   request <- readRequestBody (1024 * 10) -- TODO this magic number is crappy, improve
   let maybePing = Data.Aeson.decode request :: Maybe PingRequest
   maybe respondBadRequest addPing maybePing

respondWith :: MonadSnap m => Int -> m ()
respondWith = modifyResponse . setResponseCode

respondBadRequest       :: MonadSnap m => m ()
respondInternalServer   :: MonadSnap m => m ()
respondNoContent        :: MonadSnap m => m ()
respondBadRequest       = respondWith 400
respondInternalServer   = respondWith 500
respondNoContent        = respondWith 204

addPing :: PingRequest -> AppHandler ()
addPing pingRequest = do
   addedPing <- with db $ withConnection (addPingFromRequest pingRequest)
   case addedPing of
      Just _ -> respondNoContent
      Nothing -> do
         logError . textToByteString $ failedInsertPrefix `T.append` userID pingRequest
         respondInternalServer
   where
      failedInsertPrefix = T.pack "Failed to insert new ping: "

textToByteString :: T.Text -> BSC.ByteString
textToByteString = BSC.pack . T.unpack

addPingFromRequest :: PingRequest -> Connection -> IO (Maybe Integer)
addPingFromRequest pingRequest conn = 
   P.addPing conn date' tenantId' link' userID' message'
   where
      date' = pingUTCTime pingRequest
      tenantId' = tenantId pingRequest
      link' = fromMaybe nullURI $ parseURI (issueLink pingRequest)
      userID' = userID pingRequest
      message' = message pingRequest

pingUTCTime :: PingRequest -> UTCTime
pingUTCTime pingRequest = posixSecondsToUTCTime posixSeconds
   where
      posixSeconds = (realToFrac . timeStamp $ pingRequest) / 1000.0

--Returns the pingID if ping was a success
ping :: P.Ping -> IO(Maybe Integer)
ping rq = do
  putStrLn("pinged: " ++ show rq)
  return $ Just (P.pingId rq)
  
executePingsHandler:: AppHandler()
executePingsHandler = do 
  deleted <- executePings'
  modifyResponse $ setResponseCode 200
  writeText (T.pack $ show deleted )
  
executePings':: AppHandler([Integer]) 
executePings' = with db $ withConnection $ \conn ->
  do
    pings <- liftIO $ P.getPings conn
    successes <- sequence $ map ping pings
    sequence $ map (P.deletePing conn) (catMaybes successes)                                     
    return $ catMaybes successes
    
                                         
  
