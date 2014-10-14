module DatabaseSnaplet 
   ( dbInitConf
   ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Connect.Zone as CZ
import           Data.Pool
import qualified Data.EnvironmentHelpers as DE
import qualified Database.PostgreSQL.Simple as P
import qualified Data.Text as T
import           System.Environment (getEnv)
import qualified Snap.Snaplet as SS
import           Snap.Snaplet.PostgresqlSimple

dbInitConf :: Maybe CZ.Zone -> SS.SnapletInit b Postgres
dbInitConf Nothing = pgsInit
dbInitConf (Just _) = SS.makeSnaplet (T.pack "Remind Me RDS") (T.pack "Relational data store connection.") Nothing $ do
   host     <- siGetEnv "PG_REMIND_ME_HOST"
   port     <- fmap read $ siGetEnv "PG_REMIND_ME_PORT"
   schema   <- siGetEnv "PG_REMIND_ME_SCHEMA"
   role     <- siGetEnv "PG_REMIND_ME_ROLE"
   password <- siGetEnv "PG_REMIND_ME_PASSWORD"
   stripes  <- fmap read . liftIO $ DE.getEnvWithDefault "1" "PG_POOL_NUM_STRIPES"
   idle     <- fmap read . liftIO $ DE.getEnvWithDefault "5" "PG_POOL_IDLE_TIME"
   maxRes   <- fmap read . liftIO $ DE.getEnvWithDefault "20" "PG_POOL_MAX_RESOURCES_PER_STRIPE"
   let connectionInfo = P.ConnectInfo 
                           { P.connectHost = host
                           , P.connectPort = port
                           , P.connectUser = role
                           , P.connectPassword = password
                           , P.connectDatabase = schema 
                           }
   let connectionString = P.postgreSQLConnectionString connectionInfo
   let openConnection = P.connectPostgreSQL connectionString
   let closeConnection = P.close
   let ndIdleTime = fromIntegral (idle :: Integer)
   pool <- liftIO $ createPool openConnection closeConnection stripes ndIdleTime maxRes
   return $ Postgres pool
   where
      siGetEnv = liftIO . getEnv
