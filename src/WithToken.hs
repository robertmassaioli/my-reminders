module WithToken where

import           Control.Applicative ((<$>))
import           Control.Monad.IO.Class (liftIO)
import qualified Snap.AtlassianConnect as AC
import qualified Data.ByteString.Char8 as BSC
import qualified Data.CaseInsensitive as DC
import qualified Data.Time.Clock as DTC
import qualified Persistence.PostgreSQL as PP
import qualified Persistence.Tenant as TN
import qualified Snap.Core as SC
import qualified SnapHelpers as SH

import           Application

acHeaderName :: DC.CI BSC.ByteString
acHeaderName = DC.mk . BSC.pack $ "X-acpt" -- See atlassian-connect-play-java PageTokenValidatorAction#TOKEN_KEY

tenantFromToken :: (AC.TenantWithUser -> AppHandler ()) -> AppHandler ()
tenantFromToken tenantApply = do
  request <- SC.getRequest
  let potentialTokens = SC.getHeaders acHeaderName request
  case potentialTokens of
    Nothing -> SH.respondWithError SH.badRequest "You need to provide a page token in the headers to use this resource. None was provided."
    Just [acTokenHeader] -> do
      connectData <- AC.getConnect
      let potentiallyDecodedToken = AC.decryptPageToken (AC.connectAES connectData) acTokenHeader
      case potentiallyDecodedToken of
         Left errorMessage -> SH.respondWithError SH.badRequest $ "Error decoding the token you provided: " ++ errorMessage
         Right pageToken -> do
            let tokenExpiryTime = DTC.addUTCTime (fromIntegral . AC.connectPageTokenTimeout $ connectData) (AC.pageTokenTimestamp pageToken)
            currentTime <- liftIO DTC.getCurrentTime
            if DTC.diffUTCTime currentTime tokenExpiryTime < 0
               then do
                  -- TODO the token has not expired, so we need to load the tenant and give it
                  -- back to the program
                  potentialTenant <- lookupTenantWithPageToken pageToken
                  case potentialTenant of
                     Nothing -> SH.respondWithError SH.notFound "Your page token was valid but the tenant could not be found. Maybe it no longer exists."
                     Just tenant -> tenantApply tenant
               else SH.respondWithError SH.unauthorised "Your token has expired. Please refresh the page."
    Just _ -> SH.respondWithError SH.badRequest "Too many page tokens were provided in the headers. Did not know which one to choose. Invalid request."

lookupTenantWithPageToken :: AC.PageToken -> AppHandler (Maybe AC.TenantWithUser)
lookupTenantWithPageToken pageToken =
  PP.withConnection $ \conn ->
    fmap (flip (,) (AC.pageTokenUser pageToken)) <$> TN.lookupTenant conn (AC.pageTokenHost pageToken)

inSecond :: b -> a -> (a, b)
inSecond x y = (y, x)
