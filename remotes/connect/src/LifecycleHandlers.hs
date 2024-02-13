{-# LANGUAGE FlexibleContexts #-}
module LifecycleHandlers
    ( lifecycleRoutes
    , validHostName
    ) where

import           Application
import qualified Control.Arrow          as ARO
import           Control.Monad          (unless)
import           Control.Monad.Trans.Class           (lift)
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Char8  as BC
import           Data.List              (find)
import           Data.Maybe             (isJust)
import qualified Data.Text              as T
import           HandlerHelpers                      (writeError)
import qualified Network.URI            as NU
import qualified Persistence.Tenant     as PT
import qualified Snap.AtlassianConnect  as AC
import qualified SnapHelpers            as SH
import qualified TenantJWT              as WT

lifecycleRoutes :: [(BC.ByteString, AppHandler ())]
lifecycleRoutes = fmap (ARO.first BC.pack) standardHandlers

standardHandlers :: [(String, AppHandler ())]
standardHandlers =
  [ ("/installed"          , installedHandler)
  , ("/uninstalled"        , uninstalledHandler)
  ]

installedHandler :: AppHandler ()
installedHandler = maybe SH.respondBadRequest installedHandlerWithTenant =<< AC.getLifecycleResponse

writeErrorWithLogging :: AppHandler (Either (Int, String) b) -> AppHandler ()
writeErrorWithLogging m = do
   res <- m
   case res of
      Right _ -> return ()
      Left (_, msg) -> SH.logErrorS $ "Error handing lifecycle event: " ++ msg
   writeError m

installedHandlerWithTenant :: AC.LifecycleResponse -> AppHandler ()
installedHandlerWithTenant tenantInfo = writeErrorWithLogging . runExceptT $ do
   validHosts <- lift (AC.connectHostWhitelist <$> AC.getConnect)
   unless (validHostName validHosts tenantInfo) $ throwE domainNotSupported
   insertionResult <- lift $ insertTenantInfo tenantInfo
   let mappedResult = ARO.left tenantInsertErrorToHttpError insertionResult
   lift $ either (\x -> SH.logErrorS . show $ x) (const $ return ()) mappedResult
   except mappedResult
   lift $ SH.logMessageS ("Installed tenant successfully: " ++ (show $ AC.lrBaseUrl tenantInfo) ++ ", " ++ (show $ AC.lrClientKey tenantInfo))
   lift SH.respondNoContent
   where
      domainNotSupported = (SH.unauthorised, "Your domain is not supported by this addon. Please contact the developers. " ++ (show . tenantAuthority $ tenantInfo))

tenantInsertErrorToHttpError :: PT.TenantInsertError -> (Int, String)
tenantInsertErrorToHttpError e = (SH.internalServer, "Failed to insert the new tenant. Not a valid host or the tenant information was invalid: " ++ show e)

insertTenantInfo :: AC.LifecycleResponse -> AppHandler (Either PT.TenantInsertError Integer)
insertTenantInfo info = WT.withMaybeTenant $ \maybeTenantWithUser -> PT.insertTenantInformation (fst <$> maybeTenantWithUser) info

validHostName:: [AC.HostName] -> AC.LifecycleResponse -> Bool
validHostName validHosts tenantInfo = isJust maybeValidhost
   where
      authorityMatchHost auth host = T.toLower host `T.isSuffixOf` (T.toLower . T.pack . NU.uriRegName $ auth)
      maybeValidhost = do
         auth <- tenantAuthority tenantInfo
         find (authorityMatchHost auth) validHosts

tenantAuthority :: AC.LifecycleResponse -> Maybe NU.URIAuth
tenantAuthority = NU.uriAuthority . AC.getURI . AC.lrBaseUrl

uninstalledHandler :: AppHandler ()
uninstalledHandler = do
   mTenantInfo <- AC.getLifecycleResponse
   maybe SH.respondBadRequest uninstalledHandlerWithTenant mTenantInfo

uninstalledHandlerWithTenant :: AC.LifecycleResponse -> AppHandler ()
uninstalledHandlerWithTenant tenantInfo = do
   potentialTenant <- PT.lookupTenant (AC.lrClientKey tenantInfo)
   case potentialTenant of
      Nothing -> SH.respondWithError SH.notFound "Tried to uninstall a tenant that did not even exist."
      Just tenant -> PT.hibernateTenant tenant >> SH.respondNoContent
