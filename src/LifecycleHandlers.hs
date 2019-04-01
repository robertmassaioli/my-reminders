{-# LANGUAGE FlexibleContexts #-}
module LifecycleHandlers
    ( lifecycleRoutes
    , validHostName
    ) where

import           Application
import           Control.Applicative
import qualified Control.Arrow          as ARO
import qualified Data.ByteString.Char8  as BC
import           Data.List              (find)
import           Data.Maybe             (isJust)
import qualified Data.Text              as T
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

installedHandlerWithTenant :: AC.LifecycleResponse -> AppHandler ()
installedHandlerWithTenant tenantInfo = do
   validHosts <- fmap AC.connectHostWhitelist AC.getConnect
   if validHostName validHosts tenantInfo
      then insertTenantInfo tenantInfo >>= maybe tenantInsertionFailedResponse (const SH.respondNoContent)
      else domainNotSupportedResponse
   where
      tenantInsertionFailedResponse = SH.respondWithError SH.unauthorised "Failed to insert the new tenant. Not a valid host or the tenant information was invalid."
      domainNotSupportedResponse = SH.respondWithError SH.unauthorised $ "Your domain is not supported by this addon. Please contact the developers. " ++ (show . tenantAuthority $ tenantInfo)

insertTenantInfo :: AC.LifecycleResponse -> AppHandler (Maybe Integer)
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
