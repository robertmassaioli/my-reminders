{-# LANGUAGE FlexibleContexts #-}

module Connect.Routes
  ( connectRoutes
  , homeHandler
  ) where

import qualified AtlassianConnect as AC
import Application
import Persistence.Tenant
import Persistence.PostgreSQL -- TODO dependency tree of my modules to see the flow
import qualified Control.Applicative as CA
import qualified Control.Arrow as ARO
import qualified Control.Monad as CM
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Network.HTTP.Media.MediaType as NM

import qualified Snap.Snaplet as SS
import qualified Snap.Core as SC

import qualified Data.ByteString.Char8 as BLC
import qualified Data.CaseInsensitive as CI

import qualified Connect.Data as CD
import qualified SnapHelpers as SH

data Protocol = HTTP | HTTPS

instance Show (Protocol) where
  show HTTP = "http"
  show HTTPS = "https"

type Port = Int

serverPortSuffix :: Protocol -> Port -> BLC.ByteString
serverPortSuffix HTTP  port = BLC.pack $ if port /= 0 && port /= 80 then ":" ++ show port else ""
serverPortSuffix HTTPS port = BLC.pack $ if port /= 0 && port /= 443 then ":" ++ show port else ""

homeHandler :: CD.HasConnect (SS.Handler b v) => SS.Handler b v () -> SS.Handler b v ()
homeHandler sendHomePage = ourAccept jsonMT sendJson CA.<|> ourAccept textHtmlMT sendHomePage
  where
    sendJson = SC.method SC.GET atlassianConnectHandler CA.<|> SH.respondWithError SH.badRequest "You can only GET the atlassian connect descriptor."
    (Just jsonMT) = parseString "application/json"
    (Just textHtmlMT) = parseString "text/html"

-- TODO this should be a helper function somewhere
parseString :: String -> Maybe NM.MediaType
parseString = NM.parse . BLC.pack

ourAccept :: NM.MediaType -> SS.Handler b v () -> SS.Handler b v ()
ourAccept mediaType action = do
  request <- SC.getRequest
  CM.unless (SC.getHeader bsAccept request == (Just . NM.toByteString $ mediaType)) SC.pass
  action
  where
    bsAccept :: CI.CI BLC.ByteString
    bsAccept = CI.mk . BLC.pack $ "Accept"

connectRoutes :: [(BLC.ByteString, SS.Handler App App ())]
connectRoutes = fmap (ARO.first BLC.pack) simpleConnectRoutes

simpleConnectRoutes :: [(String, SS.Handler App App ())]
simpleConnectRoutes =
  [ ("/atlassian-connect.json" , atlassianConnectHandler)
  , ("/installed"          , installedHandler)
  --, ("/uninstalled"        , uninstalledHandler
  ]

atlassianConnectHandler :: (CD.HasConnect (SS.Handler b v)) => SS.Handler b v ()
atlassianConnectHandler = do
  connectData <- CD.getConnect
  request <- SC.getRequest
  let dc = AC.DescriptorConfig
          { AC.dcPluginName = T.pack . CD.connectPluginName $ connectData
          , AC.dcPluginKey = T.pack . CD.connectPluginKey $ connectData
          , AC.dcBaseUrl = resolveBaseUrl request
          }
  writeJson . AC.addonDescriptor $ dc

installedHandler :: AppHandler ()
installedHandler = do
   request <- SC.readRequestBody (1024 * 10)
   let mTenantInfo = A.decode request :: Maybe LifecycleResponse
   maybe (SC.modifyResponse $ SC.setResponseCode 400) (\tenantInfo -> do
       --isValidPubKey <- checkPubKey $ publicKey tenant
       mTenantId <- SS.with db $ withConnection $ \conn -> insertTenantInformation conn tenantInfo
       case mTenantId of
          Just _ -> SC.modifyResponse $ SC.setResponseCode 204
          Nothing -> SH.respondWithError SH.internalServer "Failed to insert the new tenant."
      ) mTenantInfo

-- TODO extract this into a helper module
writeJson :: (SC.MonadSnap m, A.ToJSON a) => a -> m ()
writeJson a = do
   SC.modifyResponse . SC.setContentType . BLC.pack $ "application/json"
   SC.writeLBS $ A.encode a

-- TODO extract into helper module
resolveBaseUrl :: SC.Request -> BLC.ByteString
resolveBaseUrl req =
   let serverName = SC.rqServerName req
       serverPort = SC.rqServerPort req
       proto = if SC.rqIsSecure req then HTTPS else HTTP
   in toAbsoluteUrl proto serverName serverPort

-- |
-- >>> toAbsoluteUrl "http" "example.com" 9000
-- http://example.com:9000/
toAbsoluteUrl :: Protocol -> BLC.ByteString -> Int -> BLC.ByteString
toAbsoluteUrl protocol serverName port =
  bsProtocol `BLC.append` protocolSeparator `BLC.append` serverName `BLC.append` serverPortSuffix protocol port
  where
    bsProtocol = BLC.pack $ show protocol
    protocolSeparator = BLC.pack "://"
