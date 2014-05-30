{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}

module PingHandlers(pingMe, executePings) where

import Data.Text
import Snap.Core
import Snap.Snaplet
import Data.Aeson
import Data.Maybe
import Application
import Data.Time.Clock
import Snap.Snaplet.PostgresqlSimple
import Database.PostgreSQL.Simple
import Persistence.PostgreSQL
import Persistence.Ping
import GHC.Generics

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

pingMe:: AppHandler ()
pingMe = do
  request <- readRequestBody (1024 * 10)
  let maybePing = Data.Aeson.decode request :: Maybe PingRequest
  maybe (modifyResponse $ setResponseCode 400) 
    (\ping -> do
        let link' = undefined                                             
        let date' = intToUTCTime $ timeStamp ping
        pingId <- with db $ withConnection $ \conn -> addPing conn date' (tenantId ping) link' (userID ping) (message ping)  
        case maybePing of
          Just _ -> modifyResponse $ setResponseCode 204
          Nothing -> do
            logError "Failed to insert new tenant"
            modifyResponse $ setResponseCode 500
    ) maybePing
                                                   
intToUTCTime:: Integer -> UTCTime
intToUTCTime = undefined

executePings = undefined
