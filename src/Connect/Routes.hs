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
import qualified Data.Text.Encoding as E
import qualified Network.HTTP.Media.MediaType as NM

import qualified Snap.Snaplet as SS
import qualified Snap.Core as SC
import qualified Snap.Snaplet.Heist as SSH

import qualified Data.ByteString.Char8 as BLC
import qualified Data.CaseInsensitive as CI

import qualified Connect.Data as CD

homeHandler :: CD.HasConnect (SS.Handler b v) => SS.Handler b v () -> SS.Handler b v ()
homeHandler sendHomePage = ourAccept jsonMT sendJson CA.<|> ourAccept textHtmlMT sendHomePage
  where 
    sendJson = SC.method SC.GET atlassianConnectHandler CA.<|> showError unknownHeaderMessage
    unknownHeaderMessage = T.pack "Unknown accept header"
    (Just jsonMT) = parseString "application/json"
    (Just textHtmlMT) = parseString "text/html"

-- TODO this should be a helper function somewhere
parseString :: String -> Maybe NM.MediaType
parseString = NM.parse . BLC.pack

showError :: SC.MonadSnap m => T.Text -> m ()
showError msg = do
  SC.logError $ E.encodeUtf8 msg
  SC.modifyResponse $ SC.setResponseCode 400
  SC.writeText msg

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
  connect <- CD.getConnect
  request <- SC.getRequest
  let dc = AC.DescriptorConfig
          { AC.dcPluginName = T.pack . CD.connectPluginName $ connect
          , AC.dcPluginKey = T.pack . CD.connectPluginKey $ connect
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
          Nothing -> do
           SC.logError . BLC.pack $ "Failed to insert new tenant"
           SC.modifyResponse $ SC.setResponseCode 500
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
       proto = if SC.rqIsSecure req then "https" else "http"
   in toAbsoluteUrl proto serverName serverPort

-- |
-- >>> toAbsoluteUrl "http" "example.com" 9000
-- http://example.com:9000/
toAbsoluteUrl :: String -> BLC.ByteString -> Int -> BLC.ByteString
toAbsoluteUrl proto serverName port = 
  bsProto `BLC.append` protocolSeparator `BLC.append` serverName `BLC.append` serverPortSuffix proto
  where
    serverPortSuffix :: String -> BLC.ByteString
    serverPortSuffix "http"  = BLC.pack $ if port /= 0 && port /= 80 then ":" ++ show port else ""
    serverPortSuffix "https" = BLC.pack $ if port /= 0 && port /= 443 then ":" ++ show port else ""

    bsProto = BLC.pack proto
    protocolSeparator = BLC.pack "://"
