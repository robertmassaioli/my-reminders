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

import           AdminHandlers
import           Application
import           Control.Monad.IO.Class                      (liftIO)
import           CustomSplices
import           Data.ByteString                             (ByteString)
import           Data.Maybe                                  (fromMaybe)
import           ExpireHandlers
import           Healthcheck
import           Heartbeat
import           MigrationHandler
import           PurgeHandlers
import           ReminderHandlers
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           StaticSnaplet
import           SnapHelpers
import           StatisticsHandlers
import           WebhookHandlers
import qualified AppConfig                                   as CONF
import qualified AtlassianConnect                            as AC
import qualified Control.Monad                               as CM
import qualified Data.CaseInsensitive                        as CI
import qualified Data.EnvironmentHelpers                     as DE
import qualified Data.Text                                   as T
import qualified Data.Text.IO                                as T
import qualified DatabaseSnaplet                             as DS
import qualified LifecycleHandlers                           as LH
import qualified MicrosZone                                  as MZ
import qualified Network.Cryptor                             as NC
import qualified Snap.AtlassianConnect                       as AC
import qualified Snap.Core                                   as SC
import qualified Snap.Snaplet                                as SS
import qualified Snap.Snaplet.Heist                          as SSH
import qualified SnapHelpers                                 as SH
import qualified TenantJWT                                   as TJ
import qualified Text.HTML.TagSoup                           as TS

import qualified Paths_my_reminders                          as PMR

sendHomePage :: SS.Handler b v ()
sendHomePage = SC.redirect' "/docs/home" SH.temporaryRedirect

readIndex :: IO T.Text
readIndex = T.readFile "frontend/build/index.html"

showDocPage :: SSH.HasHeist b => SS.Handler b v ()
showDocPage = do
  SC.modifyResponse (SC.setHeader (CI.mk "Content-Type") "text/html; charset=utf-8")
  SC.writeText =<< liftIO readIndex

-- TODO needs the standard page context with the base url. How do you do configuration
-- settings with the Snap framework? I think that the configuration settings should all
-- be in the database and that it is loaded once on startup and cached within the application
-- forever more.
loadConnectPanel :: AppHandler ()
loadConnectPanel = withTokenAndTenant $ \token (tenant, userKey) -> do
  connectData <- AC.getConnect
  rawHtmlContent <- liftIO readIndex
  let updatedHtmlContent = TS.renderTags . injectTags (metaTags connectData tenant token userKey) . TS.parseTags $ rawHtmlContent
  SC.modifyResponse (SC.setHeader (CI.mk "Content-Type") "text/html; charset=utf-8")
  SC.writeText updatedHtmlContent
  where
    injectTags :: [MetaTag] -> [TS.Tag T.Text] -> [TS.Tag T.Text]
    injectTags mts tags = preHead ++ inject postHead (toMetaTags mts)
      where
        (preHead, postHead) = span (not . TS.isTagOpenName "head") tags

    inject :: [TS.Tag T.Text] -> [TS.Tag T.Text] -> [TS.Tag T.Text]
    inject (headTag : afterHead) cmTags = (headTag : cmTags) ++ afterHead
    inject [] cmTags = cmTags

    toMetaTags :: [MetaTag] -> [TS.Tag T.Text]
    toMetaTags = concat . map toMetaTag
      where
        toMetaTag mt =
            [ TS.TagOpen "meta" [("name", mtName mt), ("content", mtContent mt)]
            , TS.TagClose "meta"
            ]

    metaTags connectData tenant token userKey =
      [ MT "userAccountId" (fromMaybe T.empty userKey)
      , MT "productBaseUrl" (T.pack . show . AC.baseUrl $ tenant)
      , MT "acpt" (SH.byteStringToText (AC.encryptPageToken (AC.connectAES connectData) token))
      ]

data MetaTag = MT
  { mtName :: T.Text
  , mtContent :: T.Text
  }


withTokenAndTenant :: (AC.PageToken -> AC.TenantWithUser -> AppHandler ()) -> AppHandler ()
withTokenAndTenant processor = CM.void $ TJ.withTenant $ \ct -> do
  token <- liftIO $ AC.generateTokenCurrentTime ct
  processor token ct
  return Nothing

------------------------------------------------------------------------------
-- | The application's routes.
routes :: Maybe MZ.Zone -> [(ByteString, SS.Handler App App ())]
routes zone = LH.lifecycleRoutes ++ applicationRoutes zone ++ redirects

applicationRoutes :: Maybe MZ.Zone ->  [(ByteString, SS.Handler App App ())]
applicationRoutes zone =
  [ ("/"                              , sendHomePage)
  , ("/docs/:fileparam"               , showDocPage)
  , ("/panel/v2/jira/reminder/create" , loadConnectPanel)
  , ("/panel/jira/reminder/simple"    , loadConnectPanel)
  , ("/panel/jira/reminders/view"     , loadConnectPanel)
  , ("/rest/reminder"                 , handleReminder)
  , ("/rest/reminders"                , handleReminders)
  , ("/rest/user/reminders"           , handleUserReminders)
  , ("/rest/expire"                   , handleExpireRequest)
  , ("/rest/expire/failing"           , handleExpireFailingRequest)
  , ("/rest/purge"                    , handlePurgeRequest)
  , ("/rest/webhook/issue/update"     , handleIssueUpdateWebhook)
  , ("/rest/webhook/issue/delete"     , handleIssueDeleteWebhook)
  , ("/rest/healthcheck"              , healthcheckRequest)
  , ("/rest/heartbeat"                , heartbeatRequest)
  , ("/rest/migration"                , migrationRequest zone)
  , ("/rest/statistics"               , handleStatistics)
  , ("/rest/admin/tenant/search"      , adminSearch)
  , ("/rest/admin/tenant"             , adminTenant)
  , ("/rest/"                         , respondWithError notFound "Unknown rest resource")
  , ("/robots.txt"                    , serveFile "frontend/build/robots.txt")
  , ("/favicon.ico"                   , serveFile "frontend/build/favicon.ico")
  ]

staticRoutes :: [(ByteString, SS.Handler a StaticConf ())]
staticRoutes =
  [ ("frontend"     , serveDirectory "frontend/build")
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
  SS.addRoutes $ routes zone -- Run addRoutes before heistInit: http://goo.gl/9GpeSy
  appHeist   <- SS.nestSnaplet "" heist $ SSH.heistInit "templates"
  SSH.addConfig appHeist spliceConfig
  appSession <- SS.nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)
  appDb      <- SS.nestSnaplet "db" db (DS.dbInitConf zone)
  let modifiedDescriptor = MZ.modifyDescriptorUsingZone zone AC.addonDescriptor
  appConnect <- SS.nestSnaplet "" connect (AC.initConnectSnaplet modifiedDescriptor)
  appAppConf  <- SS.nestSnaplet "rmconf" rmconf (CONF.initAppConfOrExit configDataDir)
  appStatic <- SS.nestSnaplet "static" static (initStaticSnaplet PMR.version staticRoutes appHeist)
  cryptorSnaplet <- SS.nestSnaplet "cryptor" cryptor (NC.initCryptorSnaplet zone)
  liftIO . putStrLn $ "## Ending Init Phase"
  return $ App appHeist appSession appDb appConnect appAppConf appStatic cryptorSnaplet

configDataDir :: IO String
configDataDir = CM.liftM (++ "/resources") PMR.getDataDir
