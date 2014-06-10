{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import           Snap                (get)
import           Control.Lens
import qualified Snap.Snaplet as SS
import qualified Snap.Snaplet.Heist as SSH
import           Snap.Snaplet.Session
import           Snap.Snaplet.PostgresqlSimple

import qualified Connect.Data as CD
------------------------------------------------------------------------------
data App = App
    { _heist :: SS.Snaplet (SSH.Heist App)
    , _sess  :: SS.Snaplet SessionManager
    , _db    :: SS.Snaplet Postgres
    , _connect :: SS.Snaplet CD.Connect
    }

makeLenses ''App

instance SSH.HasHeist App where
    heistLens = SS.subSnaplet heist

instance HasPostgres (SS.Handler b App) where
    getPostgresState = SS.with db get

instance CD.HasConnect (SS.Handler b App) where
   getConnect = SS.with connect get

------------------------------------------------------------------------------
type AppHandler = SS.Handler App App
