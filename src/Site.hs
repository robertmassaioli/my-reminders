{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
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
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Aeson
import           GHC.Generics
import qualified Data.Text as T
import           Snap.Core
import qualified Snap.Snaplet as SS
import qualified Heist as H
import qualified Snap.Snaplet.Heist as SSH
import qualified Database.PostgreSQL.Simple as PS
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
import qualified Heist.Interpreted as I

import qualified Web.JWT as J

import Connect.Routes
import Model.UserDetails
import Persistence.PostgreSQL
import Persistence.Ping
import qualified Persistence.Tenant as PT
import PingHandlers
import qualified TenantJWT as TJ
import qualified Connect.PageToken as CPT

writeJson :: (MonadSnap m, ToJSON a) => a -> m ()
writeJson a = do
    modifyResponse $ setContentType "application/json"
    writeLBS $ encode a

sendHomePage :: SSH.HasHeist b => SS.Handler b v ()
sendHomePage = SSH.heistLocal environment $ SSH.render "home"
   where
      environment = I.bindSplices (homeSplice getAppVersion 2 3)

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
createPingPanel = TJ.withTenant $ \tenant -> 
   SSH.heistLocal (I.bindSplices . context $ tenant) $ SSH.render "ping-create"
   where
      context tenant = do
         "productBaseUrl" H.## I.textSplice $ T.pack . show . PT.baseUrl $ tenant
         "connectBaseUrl" H.## I.textSplice $ T.pack "http://localhost:9000"

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, SS.Handler App App ())]
routes = connectRoutes ++ applicationRoutes

applicationRoutes :: [(ByteString, SS.Handler App App ())]
applicationRoutes = 
   [ ("/"                  , homeHandler sendHomePage)
   , ("/panel/ping/create" , createPingPanel )
   , ("/rest/ping"         , handlePings)  
   , ("/execute"           , executePingsHandler)  
   , ("/static"            , serveDirectory "static")
   ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SS.SnapletInit App App
app = SS.makeSnaplet "app" "ping-me connect" Nothing $ do
    h <- SS.nestSnaplet "" heist $ SSH.heistInit "templates"
    s <- SS.nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    db <- SS.nestSnaplet "db" db pgsInit
    SS.addRoutes routes
    return $ App h s db

