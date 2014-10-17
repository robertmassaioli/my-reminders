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
import           Text.PrettyPrint.Boxes

pgRemindMePre :: String -> String
pgRemindMePre = (++) "PG_REMIND_ME_"

pgPoolPre :: String -> String
pgPoolPre = (++) "PG_POOL_"

dbInitConf :: Maybe CZ.Zone -> SS.SnapletInit b Postgres
dbInitConf Nothing = pgsInit
dbInitConf (Just _) = SS.makeSnaplet (T.pack "Remind Me RDS") (T.pack "Relational data store connection.") Nothing $ do
   host     <- siGetEnv  $ pgRemindMePre "HOST"
   port     <- fmap read . siGetEnv $ pgRemindMePre "PORT"
   schema   <- siGetEnv  $ pgRemindMePre "SCHEMA"
   role     <- siGetEnv  $ pgRemindMePre "ROLE"
   password <- siGetEnv  $ pgRemindMePre "PASSWORD"
   stripes  <- fmap read . liftIO . DE.getEnvWithDefault "1"  $ pgPoolPre "NUM_STRIPES"
   idle     <- fmap read . liftIO . DE.getEnvWithDefault "5"  $ pgPoolPre "IDLE_TIME"
   maxRes   <- fmap read . liftIO . DE.getEnvWithDefault "20" $ pgPoolPre "MAX_RESOURCES_PER_STRIPE"
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
   liftIO $ printDatabaseDetails host (show port) schema role 
   pool <- liftIO $ createPool openConnection closeConnection stripes ndIdleTime maxRes
   return $ Postgres pool
   where
      siGetEnv = liftIO . getEnv

boxDatabaseDetails :: String -> String -> String -> String -> Box
boxDatabaseDetails host port schema role = 
   text "## Database Details" //
   (vcat left
      [ text " - Host:" 
      , text " - Port:"
      , text " - Schema (Database):"
      , text " - Role:"
      ]
   <+> vcat left 
      [ text host
      , text port
      , text schema
      , text role
      ]
   )

printDatabaseDetails :: String -> String -> String -> String -> IO ()
printDatabaseDetails host port schema role = printBox $ boxDatabaseDetails host port schema role
