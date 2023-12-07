{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.Reader          (local)
import           Control.Monad.State           (get)
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session
import qualified AppConfig                     as CONF
import qualified Network.Cryptor               as NC
import qualified Snap.AtlassianConnect         as AC
import qualified Snap.Snaplet                  as SS
import qualified Snap.Snaplet.Heist            as SSH
import qualified StaticSnaplet                 as STATIC
------------------------------------------------------------------------------
data App = App
  { _heist   :: SS.Snaplet (SSH.Heist App)
  , _sess    :: SS.Snaplet SessionManager
  , _db      :: SS.Snaplet Postgres
  , _connect :: SS.Snaplet AC.Connect
  , _rmconf  :: SS.Snaplet CONF.AppConf
  , _static  :: SS.Snaplet STATIC.StaticConf
  , _cryptor :: SS.Snaplet NC.CryptorConf
  }

makeLenses ''App

instance SSH.HasHeist App where
  heistLens = SS.subSnaplet heist

instance HasPostgres (SS.Handler b App) where
  getPostgresState = SS.with db get
  setLocalPostgresState s = local $ set (db . SS.snapletValue) s

instance AC.HasConnect (SS.Handler b App) where
  getConnect = SS.with connect get

instance CONF.HasAppConf (SS.Handler b App) where
  getAppConf = SS.with rmconf get

instance NC.HasCryptor (SS.Handler b App) where
  getCryptor = SS.with cryptor get

------------------------------------------------------------------------------
type AppHandler = SS.Handler App App
