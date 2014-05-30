{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}

module PingHandlers(addPingHandler, executePingsHandler) where

import qualified Data.Text as T 
import Snap.Core
import Snap.Snaplet
import Data.Aeson
import Data.Maybe
import Application
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Snap.Snaplet.PostgresqlSimple
import Database.PostgreSQL.Simple
import Persistence.PostgreSQL
import qualified Persistence.Ping as P 
import GHC.Generics
import Network.URI
import Control.Monad
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
   request <- readRequestBody (1024 * 10)
   let maybePing = Data.Aeson.decode request :: Maybe PingRequest
   maybe respondBadRequest addPing maybePing
   where
      respondBadRequest = modifyResponse . setResponseCode $ 400

      addPing :: PingRequest -> AppHandler ()
      addPing ping = do
         let link' = fromMaybe nullURI $ parseURI (issueLink ping)                                             
         let date' = posixSecondsToUTCTime $ (realToFrac $ timeStamp ping) / 1000
         addedPing <- with db $ withConnection $ 
            \conn -> P.addPing conn date' (tenantId ping) link' (userID ping) (message ping)  
         case addedPing of
            Just _ -> modifyResponse $ setResponseCode 204
            Nothing -> do
               logError "Failed to insert new tenant"
               modifyResponse $ setResponseCode 500

--Returns the pingID if ping was a success
ping:: P.Ping -> IO(Maybe Integer)
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
    
                                         
  
