module Connect.Routes 
   ( connectRoutes
   ) where

import qualified AtlassianConnect as AC
import Application
import Persistence.Tenant
import Persistence.PostgreSQL -- TODO dependency tree of my modules to see the flow
import qualified Data.Aeson as A

import qualified Snap.Snaplet as SS
import qualified Snap.Core as SC
import qualified Snap.Snaplet.Heist as SSH

import qualified Data.ByteString.Char8 as BLC

connectRoutes :: [(BLC.ByteString, SS.Handler App App ())]
connectRoutes = fmap (\(name, handler) -> (BLC.pack name, handler)) simpleConnectRoutes

simpleConnectRoutes :: [(String, SS.Handler App App ())]
simpleConnectRoutes = 
   [ ("/atlassian-connect.json" , atlassianConnectHandler)
   , ("/installed"              , installedHandler)
   --, ("/uninstalled"            , uninstalledHandler
   ]

atlassianConnectHandler :: SSH.HasHeist b => SS.Handler b v ()
atlassianConnectHandler = writeJson . AC.addonDescriptor . resolveBaseUrl =<< SC.getRequest

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
