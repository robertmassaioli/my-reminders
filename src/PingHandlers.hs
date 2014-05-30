{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}

module PingHandlers(addPingHandler, executePings) where

import Data.Text
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
import Persistence.Ping
import GHC.Generics
import Network.URI

data PingRequest = PingRequest {
                               userID:: Text,
                               timeStamp :: Integer,
                               issueLink:: String,
                               message:: Text,
                               tenantId:: Integer --TODO: this needs to be based on something
                               }
                   deriving (Show,Generic)

instance FromJSON PingRequest
instance ToJSON PingRequest

addPingHandler:: AppHandler ()
addPingHandler = do
  request <- readRequestBody (1024 * 10)
  let maybePing = Data.Aeson.decode request :: Maybe PingRequest
  maybe (modifyResponse $ setResponseCode 400) 
    (\ping -> do
        let link' = fromMaybe nullURI $ parseURI (issueLink ping)                                             
        let date' = posixSecondsToUTCTime $ (realToFrac $ timeStamp ping) / 1000
        pingId <- with db $ withConnection $ \conn -> addPing conn date' (tenantId ping) link' (userID ping) (message ping)  
        case maybePing of
          Just _ -> modifyResponse $ setResponseCode 204
          Nothing -> do
            logError "Failed to insert new tenant"
            modifyResponse $ setResponseCode 500
    ) maybePing
                                                   

executePings = undefined
