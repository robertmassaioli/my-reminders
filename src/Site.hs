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
import           Control.Applicative
import           Control.Monad (join, guard)
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.State.Class as MS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Aeson
import           Data.Monoid (mempty)
import           GHC.Generics
import qualified Data.Text as T
import           Snap.Core
import qualified Snap.Types as ST
import qualified Snap.Snaplet as SS
import qualified Heist as H
import qualified Heist.Interpreted as HI
import qualified Heist.Splices as HS
import qualified Snap.Snaplet.Heist as SSH
import qualified Database.PostgreSQL.Simple as PS
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
import qualified Heist.Interpreted as I
import qualified Text.XmlHtml as X

import qualified Web.JWT as J

import Connect.Routes
import qualified Connect.Connect as CC
import qualified Connect.Data as CD
import Model.UserDetails
import Persistence.PostgreSQL
import Persistence.Ping
import qualified Persistence.Tenant as PT
import PingHandlers
import qualified TenantJWT as TJ
import qualified Connect.Tenant as CT
import qualified WithToken as WT
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
createPingPanel = withTokenAndTenant $ \token (tenant, _) -> do
  connect <- CD.getConnect
  SSH.heistLocal (I.bindSplices $ context connect tenant token) $ SSH.render "ping-create"
  where
    context connect tenant token = do
      "productBaseUrl" H.## I.textSplice $ T.pack . show . PT.baseUrl $ tenant
      "connectBaseUrl" H.## I.textSplice $ T.pack "http://localhost:9000" -- TODO what is the best way to load your own hostname? Is this even required?
      "connectPageToken" H.## I.textSplice $ SH.byteStringToText (CPT.encryptPageToken (CC.connectAES connect) token) 

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
  , ("/panel/ping/create" , createPingPanel )
  , ("/rest/ping"         , handlePings)  
  , ("/rest/pings"        , handleMultiPings)
  , ("/static"            , serveDirectory "static")
  ]

heistConfig = mempty
   { H.hcInterpretedSplices = do
      "hasSplice" H.## hasSplice
      H.defaultInterpretedSplices
   }

------------------------------------------------------------------------------
-- | The application initializer.
app :: SS.SnapletInit App App
app = SS.makeSnaplet "app" "ping-me connect" Nothing $ do
  h <- SS.nestSnaplet "" heist $ SSH.heistInit' "templates" heistConfig
  s <- SS.nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" (Just 3600)
  db <- SS.nestSnaplet "db" db pgsInit
  connect <- SS.nestSnaplet "connect" connect CC.initConnectSnaplet
  SS.addRoutes routes
  return $ App h s db connect
