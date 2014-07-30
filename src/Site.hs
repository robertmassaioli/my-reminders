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
import           Data.Monoid (mempty)
import           Data.Maybe
import qualified Data.Text as T
import qualified Snap.Snaplet as SS
import qualified Heist as H
import qualified Heist.Interpreted as HI
import qualified Snap.Snaplet.Heist as SSH
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
import qualified Heist.Interpreted as I
import qualified Text.XmlHtml as X


import Connect.Routes
import qualified Connect.Connect as CC
import qualified Connect.Data as CD
import qualified Persistence.Tenant as PT
import qualified RemindMeConfiguration as RC
import           PingHandlers
import           ExpireHandlers
import qualified TenantJWT as TJ
import qualified Connect.Tenant as CT
import qualified Connect.PageToken as CPT
import qualified SnapHelpers as SH

sendHomePage :: SSH.HasHeist b => SS.Handler b v ()
sendHomePage = SSH.heistLocal environment $ SSH.render "home"
  where environment = I.bindSplices (homeSplice getAppVersion 2 3)

homeSplice :: Monad n => T.Text -> Int -> Int -> H.Splices (I.Splice n)
homeSplice version2 avatarSize pollerInterval = do
  "version" H.## I.textSplice version2
  "avatarSize" H.## I.textSplice $ T.pack $ show avatarSize
  "pollerInterval" H.## I.textSplice $ T.pack $ show pollerInterval

getAppVersion :: T.Text
getAppVersion = "0.1"

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
  SSH.heistLocal (I.bindSplices $ context connectData tenant token userKey) $ SSH.render panelTemplate
  where
    context connectData tenant token userKey = do
      "productBaseUrl" H.## I.textSplice $ T.pack . show . PT.baseUrl $ tenant
      "connectPageToken" H.## I.textSplice $ SH.byteStringToText (CPT.encryptPageToken (CC.connectAES connectData) token)
      "userKey" H.## I.textSplice $ maybe T.empty T.pack userKey

hasSplice :: SSH.SnapletISplice App
hasSplice = do
   potentialTokenName <- fmap (X.getAttribute "name") H.getParamNode
   case potentialTokenName of
      Just tokenName -> do
         tokenSplice <- fmap (HI.lookupSplice tokenName) H.getHS
         case tokenSplice of
            Just _ -> HI.runChildren
            Nothing -> return . comment . T.pack $ "Could not find the variable '" ++ show tokenName ++ "' in the heist context."
      Nothing -> return . comment $ "Could not find 'name' attribute."
   where
      comment x = [X.Comment x]

withTokenAndTenant :: (CPT.PageToken -> CT.ConnectTenant -> AppHandler ()) -> AppHandler ()
withTokenAndTenant processor = TJ.withTenant $ \ct -> do
  token <- liftIO $ CPT.generateTokenCurrentTime ct
  processor token ct

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, SS.Handler App App ())]
routes = connectRoutes ++ applicationRoutes

applicationRoutes :: [(ByteString, SS.Handler App App ())]
applicationRoutes =
  [ ("/"                  , homeHandler sendHomePage)
  , ("/panel/jira/ping/create" , createPingPanel )
  , ("/panel/jira/reminders/view", viewRemindersPanel)
  , ("/rest/ping"         , handlePings)
  , ("/rest/pings"        , handleMultiPings)
  , ("/rest/user/reminders", handleUserReminders)
  , ("/rest/expire"       , handleExpireRequest)
  , ("/static"            , serveDirectory "static")
  ]

heistConfig :: H.HeistConfig (SS.Handler App App)
heistConfig = mempty
   { H.hcInterpretedSplices = do
      "hasSplice" H.## hasSplice
      H.defaultInterpretedSplices
   }

------------------------------------------------------------------------------
-- | The application initializer.
app :: SS.SnapletInit App App
app = SS.makeSnaplet "app" "ping-me connect" Nothing $ do
  appHeist   <- SS.nestSnaplet "" heist $ SSH.heistInit' "templates" heistConfig
  appSession <- SS.nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" (Just 3600)
  appDb      <- SS.nestSnaplet "db" db pgsInit
  appConnect <- SS.nestSnaplet "connect" connect CC.initConnectSnaplet
  appRMConf  <- SS.nestSnaplet "rmconf" rmconf RC.initRMConf 
  SS.addRoutes routes
  return $ App appHeist appSession appDb appConnect appRMConf
