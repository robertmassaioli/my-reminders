{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Snap.Core as SC
import qualified Snap.Snaplet as SS
import qualified Heist as H
import qualified Heist.Interpreted as HI
import qualified Snap.Snaplet.Heist as SSH
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application


import           Connect.Routes
import qualified Connect.Connect as CC
import qualified Connect.Data as CD
import qualified Connect.Zone as CZ
import qualified Data.EnvironmentHelpers as DE
import qualified DatabaseSnaplet as DS
import qualified Persistence.Tenant as PT
import qualified RemindMeConfiguration as RC
import           CustomSplices
import           PingHandlers
import           ExpireHandlers
import           PurgeHandlers
import           Healthcheck
import           Heartbeat
import qualified TenantJWT as TJ
import qualified Connect.Tenant as CT
import qualified Connect.PageToken as CPT
import qualified SnapHelpers as SH

sendHomePage :: SS.Handler b v ()
sendHomePage = do
   request <- SC.getRequest
   liftIO . putStrLn $ "sendHomePage: " ++ show request
   pattern <- SS.getRoutePattern
   liftIO $ print pattern
   SC.redirect' "/docs/home" SH.temporaryRedirect

showDocPage :: SSH.HasHeist b => SS.Handler b v ()
showDocPage = do
   request <- SC.getRequest
   liftIO . putStrLn $ "showDocPage: " ++ show request
   pattern <- SS.getRoutePattern
   liftIO $ print pattern
   fileName <- SC.getParam "fileparam"
   case fileName of
      Nothing -> SH.respondNotFound
      Just rawFileName -> SSH.heistLocal (environment . T.pack . BC.unpack $ rawFileName) $ SSH.render "docs"
   where
      environment fileName = HI.bindSplices $ do
         "fileName" H.## HI.textSplice fileName

-- TODO needs the standard page context with the base url. How do you do configuration
-- settings with the Snap framework? I think that the configuration settings should all
-- be in the database and that it is loaded once on startup and cached within the application
-- forever more.
createPingPanel :: AppHandler ()
createPingPanel = createConnectPanel "ping-create"

viewRemindersPanel :: AppHandler ()
viewRemindersPanel = createConnectPanel "view-jira-reminders"

createConnectPanel :: ByteString -> AppHandler ()
createConnectPanel panelTemplate = withTokenAndTenant $ \token (tenant, userKey) -> do
  connectData <- CD.getConnect
  SSH.heistLocal (HI.bindSplices $ context connectData tenant token userKey) $ SSH.render panelTemplate
  where
    context connectData tenant token userKey = do
      "productBaseUrl" H.## HI.textSplice $ T.pack . show . PT.baseUrl $ tenant
      "connectPageToken" H.## HI.textSplice $ SH.byteStringToText (CPT.encryptPageToken (CC.connectAES connectData) token)
      "userKey" H.## HI.textSplice $ maybe T.empty T.pack userKey

         
withTokenAndTenant :: (CPT.PageToken -> CT.ConnectTenant -> AppHandler ()) -> AppHandler ()
withTokenAndTenant processor = TJ.withTenant $ \ct -> do
  token <- liftIO $ CPT.generateTokenCurrentTime ct
  processor token ct

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, SS.Handler App App ())]
routes = connectRoutes ++ applicationRoutes ++ redirects

applicationRoutes :: [(ByteString, SS.Handler App App ())]
applicationRoutes =
  [ ("$"                             , homeHandler sendHomePage)
  , ("/docs/:fileparam"             , showDocPage)
 -- [ ("/docs/:fileparam"             , showDocPage)
  , ("/panel/jira/ping/create"      , createPingPanel )
  , ("/panel/jira/reminders/view"   , viewRemindersPanel)
  , ("/rest/ping"                   , handlePings)
  , ("/rest/pings"                  , handleMultiPings)
  , ("/rest/user/reminders"         , handleUserReminders)
  , ("/rest/expire"                 , handleExpireRequest)
  , ("/rest/purge"                  , handlePurgeRequest)
  , ("/rest/healthcheck"            , healthcheckRequest)
  , ("/rest/heartbeat"              , heartbeatRequest)
  , ("/static/css"                  , serveDirectory "static/css")
  , ("/static/images"               , serveDirectory "static/images")
  , ("/static/js"                   , serveDirectory "static-js")
  ]

-- We should always redirect to external services or common operations, that way when we want to
-- change where that points to, we only have to quickly update those links here
redirects :: [(ByteString, SS.Handler App App ())]
redirects = 
   [ ("/redirect/raise-issue", SC.redirect "https://bitbucket.org/eerok/ping-me-connect/issues")
   , ("/redirect/install", SC.redirect "https://marketplace.atlassian.com/plugins/com.atlassian.ondemand.remindme")
   ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SS.SnapletInit App App
app = SS.makeSnaplet "app" "ping-me connect" Nothing $ do
  liftIO . putStrLn $ "## Starting Init Phase"
  zone <- liftIO CZ.fromEnv
  liftIO . putStrLn $ "## Zone: " ++ DE.showMaybe zone
  appHeist   <- SS.nestSnaplet "" heist $ SSH.heistInit "templates"
  SSH.addConfig appHeist spliceConfig
  SS.addRoutes routes
  appSession <- SS.nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" (Just 3600)
  appDb      <- SS.nestSnaplet "db" db (DS.dbInitConf zone)
  appConnect <- SS.nestSnaplet "connect" connect CC.initConnectSnaplet
  appRMConf  <- SS.nestSnaplet "rmconf" rmconf RC.initRMConfOrExit
  liftIO . putStrLn $ "## Ending Init Phase"
  return $ App appHeist appSession appDb appConnect appRMConf
