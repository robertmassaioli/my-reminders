module DatabaseSnaplet
   ( dbInitConf
   ) where

import           Control.Monad.IO.Class        (liftIO)
import qualified Data.EnvironmentHelpers       as DE
import           Data.Pool
import qualified Data.Text                     as T
import qualified Database.PostgreSQL.Simple    as P
import qualified MicrosZone                    as MZ
import qualified Snap.Snaplet                  as SS
import           Snap.Snaplet.PostgresqlSimple
import           System.Environment            (getEnv)
import           Text.PrettyPrint.Boxes

pgMyRemindersPre :: String -> String
pgMyRemindersPre = (++) "PG_ENCRYPTED_MY_REMINDERS_"

pgPoolPre :: String -> String
pgPoolPre = (++) "PG_POOL_"

dbInitConf :: Maybe MZ.Zone -> SS.SnapletInit b Postgres
dbInitConf Nothing = pgsInit
dbInitConf (Just _) = SS.makeSnaplet (T.pack "My Reminders RDS") (T.pack "Relational data store connection.") Nothing $ do
   host     <- siGetEnv  $ pgMyRemindersPre "HOST"
   port     <- fmap read . siGetEnv $ pgMyRemindersPre "PORT"
   schema   <- siGetEnv  $ pgMyRemindersPre "SCHEMA"
   role     <- siGetEnv  $ pgMyRemindersPre "ROLE"
   password <- siGetEnv  $ pgMyRemindersPre "PASSWORD"
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
   return $ PostgresPool pool
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
