{-# LANGUAGE OverloadedStrings #-}

module ExpireHandlers 
   ( handleExpireRequest
   ) where

import           Application
import           Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as BC
import qualified RemindMeConfiguration as RC
import qualified Snap.Core as SC
import           SnapHelpers

handleExpireRequest :: AppHandler ()
handleExpireRequest = handleMethods
   [ (SC.PUT, expireForTimestamp)
   ]

-- TODO we expect that we will be given a timestamp and that we can expire everything before that
-- timestamp.
-- TODO extra: we should check to make sure that the timestamp given is reasonably close to the
-- current timestamp (within the day). 
-- TODO Each timestamp should only be processed once. Need to ensure that this is thread safe.
-- TODO a hash should need to be provided such that only those that have the hash can trigger the
-- rest calls.
expireForTimestamp :: AppHandler ()
expireForTimestamp = do
   potentialExpireKey <- SC.getQueryParam "key"
   potentialRawTimestamp <- SC.getQueryParam "timestamp"
   let potentialTimestamp = (read . BC.unpack) <$> potentialRawTimestamp :: Maybe Integer
   case (potentialExpireKey, potentialTimestamp) of
      (Nothing, _) -> respondWithError forbidden "Speak friend and enter. However: http://i.imgur.com/fVDH5bN.gif"
      (_      , Nothing) -> respondWithError badRequest "You need to provide a timestamp for expiry to work."
      (Just expireKey, Just timestamp) -> do
         conf <- RC.getRMConf
         return ()
         -- check to ensure that the key is correct, if not then show a forbidden
         -- otherwise expire the tokens that need to be expired, do so in a brand new method
