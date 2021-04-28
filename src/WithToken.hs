module WithToken where

import           Application
import           Control.Monad               (unless)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Except
import           Data.MaybeUtil
import           HandlerHelpers              (writeError, withErr)
import qualified Data.ByteString.Char8       as BSC
import qualified Data.CaseInsensitive        as DC
import qualified Data.Time.Clock             as DTC
import qualified Snap.AtlassianConnect       as AC
import qualified Snap.Core                   as SC
import qualified SnapHelpers                 as SH
import qualified TenantPopulator             as TP

acHeaderName :: DC.CI BSC.ByteString
acHeaderName = DC.mk . BSC.pack $ "X-acpt" -- See atlassian-connect-play-java PageTokenValidatorAction#TOKEN_KEY

tenantFromToken :: (AC.TenantWithUser -> AppHandler ()) -> AppHandler ()
tenantFromToken tenantApply = writeError . runExceptT $ do
   acTokenHeader <- ExceptT (m2e noPageToken . SC.getHeader acHeaderName <$> SC.getRequest)
   connectData <- lift AC.getConnect
   pageToken <- except (handleFailedDecrypt $ AC.decryptPageToken (AC.connectAES connectData) acTokenHeader)
   let tokenExpiryTime = DTC.addUTCTime (getTokenTimeout connectData) (AC.pageTokenTimestamp pageToken)
   currentTime <- lift . liftIO $ DTC.getCurrentTime
   unless (DTC.diffUTCTime currentTime tokenExpiryTime < 0) $ throwE tokenExpired
   tenant <- ExceptT $ lookupTenantWithPageToken pageToken
   lift . tenantApply $ tenant
   where
      noPageToken = (SH.badRequest, "You need to provide a page token in the headers to use this resource. None was provided.")
      handleFailedDecrypt = withErr SH.badRequest "Error decoding the token you provided:"
      tokenExpired = (SH.unauthorised, "Your token has expired. Please refresh the page.")

      getTokenTimeout = fromIntegral . AC.connectPageTokenTimeout

lookupTenantWithPageToken :: AC.PageToken -> AppHandler (Either (Int, String) AC.TenantWithUser)
lookupTenantWithPageToken pageToken = fmap (flip (,) user) <$> TP.loadTenant host
      where
         host = AC.pageTokenHost pageToken
         user = AC.pageTokenUser pageToken
