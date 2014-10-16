{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
module Connect.Routes
  ( connectRoutes
  , homeHandler
#ifndef TESTING
  , validHostName
#endif
  ) where

import           Application
import qualified AtlassianConnect             as AC
import qualified Connect.Data                 as CD
import           Connect.Descriptor           (Name (..))
import           Control.Applicative
import qualified Control.Arrow                as ARO
import qualified Control.Monad                as CM
import qualified Data.Aeson                   as A
import qualified Data.ByteString.Char8        as BC
import qualified Data.ByteString.Lazy.Char8   as BLC
import qualified Data.CaseInsensitive         as CI
import           Data.Char                    as DC
import           Data.List
import           Data.Maybe                   (isJust)
import qualified Data.Text                    as T
import qualified Network.HTTP.Media.MediaType as NM
import           Network.URI
import           Persistence.PostgreSQL
import           Persistence.Tenant
import qualified Snap.Core                    as SC
import qualified Snap.Snaplet                 as SS
import qualified SnapHelpers                  as SH

data Protocol = HTTP | HTTPS deriving (Eq)

instance Show (Protocol) where
  show HTTP = "http"
  show HTTPS = "https"

type Port = Int

data MediaType = ApplicationJson | TextHtml deriving (Eq)

instance Show (MediaType) where
  show ApplicationJson = "application/json"
  show TextHtml = "text/html"

serverPortSuffix :: Protocol -> Port -> String
serverPortSuffix HTTP  port = if port /= 0 && port /= 80 then ":" ++ show port else ""
serverPortSuffix HTTPS port = if port /= 0 && port /= 443 then ":" ++ show port else ""

homeHandler :: CD.HasConnect (SS.Handler b v) => SS.Handler b v () -> SS.Handler b v ()
homeHandler sendHomePage = ourAccept jsonMT sendJson <|> ourAccept textHtmlMT sendHomePage
  where
    sendJson = SC.method SC.GET atlassianConnectHandler <|> SH.respondWithError SH.badRequest "You can only GET the atlassian connect descriptor."
    (Just jsonMT) = parseMediaType ApplicationJson
    (Just textHtmlMT) = parseMediaType TextHtml
    parseMediaType = NM.parse . BC.pack . show

ourAccept :: NM.MediaType -> SS.Handler b v () -> SS.Handler b v ()
ourAccept mediaType action = do
  request <- SC.getRequest
  CM.unless (SC.getHeader bsAccept request == (Just . NM.toByteString $ mediaType)) SC.pass
  action
  where
    bsAccept :: CI.CI BC.ByteString
    bsAccept = CI.mk . BC.pack $ "Accept"

connectRoutes :: [(BC.ByteString, SS.Handler App App ())]
connectRoutes = fmap (ARO.first BC.pack) simpleConnectRoutes

simpleConnectRoutes :: [(String, SS.Handler App App ())]
simpleConnectRoutes =
  [ ("/atlassian-connect.json" , atlassianConnectHandler)
  , ("/installed"          , installedHandler)
  , ("/uninstalled"        , uninstalledHandler)
  ]

atlassianConnectHandler :: (CD.HasConnect (SS.Handler b v)) => SS.Handler b v ()
atlassianConnectHandler = do
  connectData <- CD.getConnect
  request <- SC.getRequest
  let dc = AC.DynamicDescriptorConfig
          { AC.dcPluginName = case CD.connectPluginName connectData of Name t -> Name t
          , AC.dcPluginKey = CD.connectPluginKey connectData
          , AC.dcBaseUrl = resolveBaseUrl request
          }
  writeJson . AC.addonDescriptor $ dc

installedHandler :: CD.HasConnect (SS.Handler b App) => SS.Handler b App ()
installedHandler = do
   request <- SC.readRequestBody (1024 * 10)
   let mTenantInfo = A.decode request :: Maybe LifecycleResponse
   maybe SH.respondBadRequest installedHandlerWithTenant mTenantInfo

installedHandlerWithTenant :: LifecycleResponse -> SS.Handler b App ()
installedHandlerWithTenant tenantInfo = do
   validHosts <- fmap CD.connectHostWhitelist CD.getConnect
   if validHostName validHosts tenantInfo
      then insertTenantInfo tenantInfo >>= maybe tenantInsertionFailedResponse (const SH.respondNoContent)
      else domainNotSupportedResponse
   where
      tenantInsertionFailedResponse = SH.respondWithError SH.internalServer "Failed to insert the new tenant. Not a valid host or the tenant information was invalid."
      domainNotSupportedResponse = SH.respondWithError SH.unauthorised $ "Your domain is not supported by this addon. Please contact the developers. " ++ (show . tenantAuthority $ tenantInfo)

insertTenantInfo :: LifecycleResponse -> SS.Handler b App (Maybe Integer)
insertTenantInfo info = SS.with db $ withConnection (`insertTenantInformation` info)

validHostName:: [CD.HostName] -> LifecycleResponse -> Bool
validHostName validHosts tenantInfo = isJust maybeValidhost 
   where
      authorityMatchHost auth host = map DC.toLower (T.unpack host) == map DC.toLower (uriRegName auth)
      maybeValidhost = do
         auth <- tenantAuthority tenantInfo
         find (authorityMatchHost auth) validHosts

tenantAuthority :: LifecycleResponse -> Maybe URIAuth
tenantAuthority = uriAuthority . lrBaseUrl

uninstalledHandler :: CD.HasConnect (SS.Handler b App) => SS.Handler b App ()
uninstalledHandler = do
   request <- SC.readRequestBody (1024 * 10)
   let mTenantInfo = A.decode request :: Maybe LifecycleResponse
   maybe SH.respondBadRequest (\tenantInfo -> do
      potentialTenant <- withConnection $ \conn -> lookupTenant conn (lrClientKey tenantInfo)
      case potentialTenant of
         Nothing -> SH.respondWithError SH.notFound "Tried to uninstall a tenant that did not even exist."
         Just tenant -> withConnection (hibernateTenant tenant) >> SH.respondNoContent
      ) mTenantInfo

-- TODO extract this into a helper module
writeJson :: (SC.MonadSnap m, A.ToJSON a) => a -> m ()
writeJson a = do
   SC.modifyResponse . SC.setContentType . BC.pack . show $ ApplicationJson
   SC.writeLBS $ A.encode a

-- TODO extract into helper module
resolveBaseUrl :: SC.Request -> URI
resolveBaseUrl req =
   let serverName = BC.unpack $ SC.rqServerName req
       serverPort = SC.rqServerPort req
       proto = if SC.rqIsSecure req then HTTPS else HTTP
   in toAbsoluteUrl proto serverName serverPort

-- |
-- >>> toAbsoluteUrl "http" "example.com" 9000
-- http://example.com:9000/
toAbsoluteUrl :: Protocol -> String -> Int -> URI
toAbsoluteUrl protocol serverName port =
  nullURI
    { uriScheme = show protocol ++ ":"
    , uriAuthority = Just URIAuth { uriUserInfo = ""
                                  , uriRegName = serverName
                                  , uriPort = serverPortSuffix protocol port }
    }
