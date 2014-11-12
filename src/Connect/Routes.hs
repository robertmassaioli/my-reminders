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
import qualified Data.Aeson                   as A
import qualified Data.ByteString.Char8        as BC
import qualified Data.CaseInsensitive         as CI
import           Data.Char                    as DC
import           Data.List
import           Data.List.Split              (splitOn)
import qualified Data.Map.Lazy                as ML
import           Data.Maybe                   (isJust, catMaybes, fromMaybe)
import qualified Data.Text                    as T
import qualified Network.HTTP.Media.MediaType as NM
import           Network.URI
import           Persistence.PostgreSQL
import           Persistence.Tenant
import qualified Snap.Core                    as SC
import qualified Snap.Snaplet                 as SS
import qualified SnapHelpers                  as SH

data MediaType = ApplicationJson | TextHtml deriving (Eq)

newtype OrdMediaType = OMT NM.MediaType deriving (Eq, Show)

instance Ord OrdMediaType where
   compare a b = compare (show a) (show b)   

instance Show (MediaType) where
  show ApplicationJson = "application/json"
  show TextHtml = "text/html"

homeHandler :: CD.HasConnect (SS.Handler b v) => SS.Handler b v () -> SS.Handler b v ()
homeHandler sendHomePage = SC.method SC.GET handleGet <|> SH.respondWithError SH.badRequest "You can only GET the homepage."
  where
    handleGet = do
      organisedHeaders <- organiseAcceptHeader
      case find isAcceptedMediaType organisedHeaders of
        Just mediaType -> fromMaybe lookupFailed (ML.lookup (OMT mediaType) responseMap) 
        Nothing -> unknownHeader

    responseMap = ML.fromList
      [ (OMT jsonMT, atlassianConnectHandler)
      , (OMT textHtmlMT, sendHomePage)
      ]

    lookupFailed = SH.respondWithError SH.internalServer "We thought that we could handle this Accept header but it turns out that we couldn't. Server error."
    unknownHeader = SH.respondWithError SH.notFound "No response to a request with the provided Accept header."

    isAcceptedMediaType = flip elem acceptedMediaTypes
    acceptedMediaTypes = [jsonMT, textHtmlMT]
    (Just jsonMT) = parseMediaType (show ApplicationJson)
    (Just textHtmlMT) = parseMediaType (show TextHtml)

parseMediaType :: String -> Maybe NM.MediaType
parseMediaType = NM.parse . BC.pack

-- An example accept header: 
-- Just "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"
organiseAcceptHeader :: SS.Handler b v [NM.MediaType]
organiseAcceptHeader = do
   request <- SC.getRequest
   case SC.getHeader bsAccept request of
      Nothing -> return []
      Just rawHeader -> return . catMaybes . fmap parseMediaType . splitOn "," . BC.unpack $ rawHeader

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
  let dc = AC.DynamicDescriptorConfig
          { AC.dcPluginName = case CD.connectPluginName connectData of Name t -> Name t
          , AC.dcPluginKey = CD.connectPluginKey connectData
          , AC.dcBaseUrl = CD.connectBaseUrl connectData
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
      authorityMatchHost auth host = T.toLower host `T.isSuffixOf` (T.toLower . T.pack . uriRegName $ auth)
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