module WithToken where

import           Control.Monad.IO.Class (liftIO)

import qualified Connect.PageToken as PT
import qualified Connect.Data as CD
import qualified Data.ByteString.Char8 as BSC
import qualified Data.CaseInsensitive as DC
import qualified Data.Text as T
import qualified Data.Time.Clock as DTC
import qualified Persistence.PostgreSQL as PP
import qualified Persistence.Tenant as TN
import qualified Snap.Core as SC
import qualified SnapHelpers as SH

import           Application

acHeaderName :: DC.CI BSC.ByteString
acHeaderName = DC.mk . BSC.pack $ "ac_token" -- See atlassian-connect-play-java Constants.java#AC_TOKEN

tenantFromToken :: (TN.Tenant -> AppHandler ()) -> AppHandler ()
tenantFromToken tenantApply = do
   request <- SC.getRequest
   let potentialTokens = SC.getHeaders acHeaderName request
   case potentialTokens of
      Nothing -> respondWithMessageAndLog missingPageTokenMessage
      Just [acTokenHeader] -> do
         connect <- CD.getConnect
         let potentiallyDecodedToken = PT.decryptPageToken (CD.connectAES connect) acTokenHeader
         case potentiallyDecodedToken of
            Left error -> respondWithMessageAndLog error
            Right pageToken -> do
               let tokenExpiryTime = DTC.addUTCTime (fromIntegral . CD.connectPageTokenTimeout $ connect) (PT.pageTokenTimestamp pageToken)
               currentTime <- liftIO DTC.getCurrentTime
               if DTC.diffUTCTime currentTime tokenExpiryTime > 0
                  then do
                     -- TODO the token has not expired, so we need to load the tenant and give it
                     -- back to the program
                     potentialTenant <- lookupTenantWithPageToken pageToken
                     case potentialTenant of
                        Nothing -> respondWithMessageAndLog "Error: Your page token was valid but the tenant could not be found. Maybe it no longer exists."
                        Just tenant -> tenantApply tenant
                  else respondWithMessageAndLog "Error: your token has expired."
      Just _ -> respondWithMessageAndLog tooManyPageTokensMessage
   where
      missingPageTokenMessage = "Error: expected page token for request and recieved none."
      tooManyPageTokensMessage = "Error: Expected only one page token but recieved several. Please only send one page token."

respondWithMessageAndLog :: String -> AppHandler ()
respondWithMessageAndLog message = do
   SH.logErrorS message
   SC.writeText . T.pack $ message
   SH.respondBadRequest

lookupTenantWithPageToken :: PT.PageToken -> AppHandler (Maybe TN.Tenant)
lookupTenantWithPageToken pageToken = do
   PP.withConnection $ \conn ->
      TN.lookupTenant conn (PT.pageTokenHost pageToken)
