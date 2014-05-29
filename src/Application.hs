{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import           Snap                (get)
import           Control.Lens
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
import           Snap.Snaplet.PostgresqlSimple
------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess  :: Snaplet SessionManager
    , _db    :: Snaplet Postgres
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasPostgres (Handler b App) where
    getPostgresState = with db get

------------------------------------------------------------------------------
type AppHandler = Handler App App


