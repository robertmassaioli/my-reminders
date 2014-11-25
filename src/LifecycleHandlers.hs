{-# LANGUAGE FlexibleContexts #-}
module LifecycleHandlers
    ( lifecycleRoutes
    , validHostName
    ) where

import Application
import qualified Control.Arrow         as ARO
import qualified Data.ByteString.Char8 as BC
import qualified Snap.Snaplet as SS
import qualified Connect.Routes as CR
import qualified SnapHelpers as SH
import qualified Connect.Data as CD
import qualified Connect.LifecycleResponse as CL
import           Data.Maybe                   (isJust)
import qualified Data.Text as T
import Persistence.PostgreSQL
import qualified Connect.Instances         as CI
import qualified Persistence.Tenant as PT
import qualified Network.URI as NU
import Data.List (find)

lifecycleRoutes :: [(BC.ByteString, SS.Handler App App ())]
lifecycleRoutes = fmap (ARO.first BC.pack) standardHandlers

standardHandlers :: [(String, SS.Handler App App ())]
standardHandlers =
  [ ("/installed"          , installedHandler)
  , ("/uninstalled"        , uninstalledHandler)
  ]

installedHandler :: CD.HasConnect (SS.Handler b App) => SS.Handler b App ()
installedHandler = maybe SH.respondBadRequest installedHandlerWithTenant =<< CR.getLifecycleResponse

installedHandlerWithTenant :: CL.LifecycleResponse -> SS.Handler b App ()
installedHandlerWithTenant tenantInfo = do
   validHosts <- fmap CD.connectHostWhitelist CD.getConnect
   if validHostName validHosts tenantInfo
      then insertTenantInfo tenantInfo >>= maybe tenantInsertionFailedResponse (const SH.respondNoContent)
      else domainNotSupportedResponse
   where
      tenantInsertionFailedResponse = SH.respondWithError SH.internalServer "Failed to insert the new tenant. Not a valid host or the tenant information was invalid."
      domainNotSupportedResponse = SH.respondWithError SH.unauthorised $ "Your domain is not supported by this addon. Please contact the developers. " ++ (show . tenantAuthority $ tenantInfo)

insertTenantInfo :: CL.LifecycleResponse -> SS.Handler b App (Maybe Integer)
insertTenantInfo info = SS.with db $ withConnection (`PT.insertTenantInformation` info)

validHostName:: [CD.HostName] -> CL.LifecycleResponse -> Bool
validHostName validHosts tenantInfo = isJust maybeValidhost
   where
      authorityMatchHost auth host = T.toLower host `T.isSuffixOf` (T.toLower . T.pack . NU.uriRegName $ auth)
      maybeValidhost = do
         auth <- tenantAuthority tenantInfo
         find (authorityMatchHost auth) validHosts

tenantAuthority :: CL.LifecycleResponse -> Maybe NU.URIAuth
tenantAuthority = NU.uriAuthority . CI.getURI . CL.lrBaseUrl

uninstalledHandler :: CD.HasConnect (SS.Handler b App) => SS.Handler b App ()
uninstalledHandler = do
   mTenantInfo <- CR.getLifecycleResponse
   maybe SH.respondBadRequest uninstalledHandlerWithTenant mTenantInfo

uninstalledHandlerWithTenant :: CL.LifecycleResponse -> SS.Handler b App ()
uninstalledHandlerWithTenant tenantInfo = do
   potentialTenant <- withConnection $ \conn -> PT.lookupTenant conn (CL.lrClientKey tenantInfo)
   case potentialTenant of
      Nothing -> SH.respondWithError SH.notFound "Tried to uninstall a tenant that did not even exist."
      Just tenant -> withConnection (PT.hibernateTenant tenant) >> SH.respondNoContent