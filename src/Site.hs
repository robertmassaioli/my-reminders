{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

import qualified AppConfig                                   as CONF
import           Application
import qualified AtlassianConnect                            as AC
import qualified Snap.AtlassianConnect as AC
import qualified Control.Monad                               as CM
import           Control.Monad.IO.Class                      (liftIO)
import           CustomSplices
import           Data.ByteString                             (ByteString)
import qualified Data.EnvironmentHelpers                     as DE
import           Data.Maybe                                  (fromMaybe)
import qualified Data.Text                                   as T
import           Data.Text.Encoding                          (decodeUtf8)
import qualified DatabaseSnaplet                             as DS
import           ExpireHandlers
import           Healthcheck
import           Heartbeat
import qualified Heist                                       as H
import qualified Heist.Interpreted                           as HI
import qualified LifecycleHandlers                           as LH
import qualified MicrosZone                                  as MZ
import           MigrationHandler
import           PurgeHandlers
import           ReminderHandlers
import qualified Snap.Core                                   as SC
import qualified Snap.Snaplet                                as SS
import qualified Snap.Snaplet.Heist                          as SSH
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import qualified SnapHelpers                                 as SH
import           StatisticsHandlers
import qualified TenantJWT                                   as TJ
import           WebhookHandlers

import qualified Paths_my_reminders                          as PMR

sendHomePage :: SS.Handler b v ()
sendHomePage = SC.redirect' "/docs/home" SH.temporaryRedirect

showDocPage :: SSH.HasHeist b => SS.Handler b v ()
showDocPage = do
   fileName <- SC.getParam "fileparam"
   case fileName of
      Nothing -> SH.respondNotFound
      Just rawFileName -> SSH.heistLocal (environment . decodeUtf8 $ rawFileName) $ SSH.render "docs"
   where
      environment fileName = HI.bindSplices $ "fileName" H.## HI.textSplice fileName

-- TODO needs the standard page context with the base url. How do you do configuration
-- settings with the Snap framework? I think that the configuration settings should all
-- be in the database and that it is loaded once on startup and cached within the application
-- forever more.
createConnectPanel :: ByteString -> AppHandler ()
createConnectPanel panelTemplate = withTokenAndTenant $ \token (tenant, userKey) -> do
  connectData <- AC.getConnect
  SSH.heistLocal (HI.bindSplices $ context connectData tenant token userKey) $ SSH.render panelTemplate
  where
    context connectData tenant token userKey = do
      "productBaseUrl" H.## HI.textSplice $ T.pack . show . AC.baseUrl $ tenant
      "connectPageToken" H.## HI.textSplice $ SH.byteStringToText (AC.encryptPageToken (AC.connectAES connectData) token)
      -- TODO The user key must be a string, this is not valid in JIRA. JIRA probably supports more varied keys
      "userKey" H.## HI.textSplice $ fromMaybe T.empty userKey


withTokenAndTenant :: (AC.PageToken -> AC.TenantWithUser -> AppHandler ()) -> AppHandler ()
withTokenAndTenant processor = TJ.withTenant $ \ct -> do
  token <- liftIO $ AC.generateTokenCurrentTime ct
  processor token ct

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, SS.Handler App App ())]
routes = LH.lifecycleRoutes ++ applicationRoutes ++ redirects

applicationRoutes :: [(ByteString, SS.Handler App App ())]
applicationRoutes =
  [ ("/"                            , SS.with connect $ AC.homeHandler sendHomePage)
  , ("/docs/:fileparam"             , showDocPage)
  , ("/panel/jira/reminder/create"  , createConnectPanel "create-reminder")
  , ("/panel/jira/reminder/simple"  , createConnectPanel "view-issue-reminders")
  , ("/panel/jira/reminders/view"   , createConnectPanel "view-jira-reminders")
  , ("/rest/reminder"               , handleReminder)
  , ("/rest/reminders"              , handleReminders)
  , ("/rest/user/reminders"         , handleUserReminders)
  , ("/rest/expire"                 , handleExpireRequest)
  , ("/rest/purge"                  , handlePurgeRequest)
  , ("/rest/webhook/issue/update"   , handleIssueUpdateWebhook)
  , ("/rest/webhook/issue/delete"   , handleIssueDeleteWebhook)
  , ("/rest/healthcheck"            , healthcheckRequest)
  , ("/rest/heartbeat"              , heartbeatRequest)
  , ("/rest/migration"              , migrationRequest)
  , ("/rest/statistics"             , handleStatistics)
  , ("/static/css"                  , serveDirectory "static/css")
  , ("/static/images"               , serveDirectory "static/images")
  , ("/static/js"                   , serveDirectory "static-js")
  , ("/robots.txt"                  , serveFile "static/files/robots.txt")
  ]

-- We should always redirect to external services or common operations, that way when we want to
-- change where that points to, we only have to quickly update those links here
redirects :: [(ByteString, SS.Handler App App ())]
redirects =
   -- Have to redirect right to the correct rapid board thanks to: https://jdog.jira-dev.com/browse/SW-1142
   [ ("/redirect/raise-issue", SC.redirect "https://ecosystem.atlassian.net/secure/RapidBoard.jspa?projectKey=MR&rapidView=189")
   , ("/redirect/install", SC.redirect "https://marketplace.atlassian.com/plugins/com.atlassian.myreminders")
   , ("/redirect/help", SC.redirect "/docs/about")
   ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SS.SnapletInit App App
app = SS.makeSnaplet "my-reminders" "My Reminders" Nothing $ do
  liftIO . putStrLn $ "## Starting Init Phase"
  zone <- liftIO MZ.fromEnv
  liftIO . putStrLn $ "## Zone: " ++ DE.showMaybe zone
  SS.addRoutes routes -- Run addRoutes before heistInit: http://goo.gl/9GpeSy
  appHeist   <- SS.nestSnaplet "" heist $ SSH.heistInit "templates"
  SSH.addConfig appHeist spliceConfig
  appSession <- SS.nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" (Just 3600)
  appDb      <- SS.nestSnaplet "db" db (DS.dbInitConf zone)
  let modifiedDescriptor = MZ.modifyDescriptorUsingZone zone AC.addonDescriptor
  appConnect <- SS.nestSnaplet "" connect (AC.initConnectSnaplet modifiedDescriptor)
  appAppConf  <- SS.nestSnaplet "rmconf" rmconf (CONF.initAppConfOrExit configDataDir)
  liftIO . putStrLn $ "## Ending Init Phase"
  return $ App appHeist appSession appDb appConnect appAppConf

configDataDir :: IO String
configDataDir = CM.liftM (++ "/resources") PMR.getDataDir
