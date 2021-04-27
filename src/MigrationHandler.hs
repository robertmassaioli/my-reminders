{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module MigrationHandler
   ( migrationRequest
   ) where

import           Application
import           Control.Monad.IO.Class                     (liftIO)
import           Control.Monad.Trans.Except
import           Data.MaybeUtil
import           Finder
import           HandlerHelpers                             (writeError)
import           System.FilePath                            ((</>))
import           Text.Read                                  (readMaybe)
import qualified AppConfig                                  as CONF
import qualified AppHelpers                                 as AH
import qualified Control.Arrow                              as A
import qualified Control.Exception                          as E
import qualified Control.Exception.Lifted                   as EL
import qualified Data.ByteString.Char8                      as BSC
import qualified Data.EnvironmentHelpers                    as DE
import qualified Database.HDBC.PostgreSQL                   as PGB
import qualified Database.Schema.Migrations.Backend.HDBC    as Backend
import qualified MicrosZone                                 as MZ
import qualified Moo.Core                                   as M
import qualified Moo.Main                                   as M
import qualified Snap.Core                                  as SC
import qualified SnapHelpers                                as SH

import           Control.Monad                              (void)
import           Database.PostgreSQL.Simple.SqlQQ
import           Snap.Snaplet.PostgresqlSimple

migrationRequest :: Maybe MZ.Zone -> AppHandler ()
migrationRequest zone = SH.handleMethods
   [ (SC.PUT, handleMigrationRun zone)
   , (SC.POST, handleInitMigrations)
   ]

handleInitMigrations :: AppHandler ()
handleInitMigrations = void $ execute_
   [sql|
   DROP TABLE IF EXISTS installed_migrations;

   CREATE TABLE public.installed_migrations (
      migration_id text
   );

   INSERT INTO public.installed_migrations VALUES ('root');
   INSERT INTO public.installed_migrations VALUES ('V1__create-atlassian-connect');
   INSERT INTO public.installed_migrations VALUES ('V2__create-remind-me-basics');
   INSERT INTO public.installed_migrations VALUES ('V3__add-in-oauth-support');
   INSERT INTO public.installed_migrations VALUES ('V4__stop-storing-user-email');
   INSERT INTO public.installed_migrations VALUES ('V5__start-storing-atlassian-account-ids');
   INSERT INTO public.installed_migrations VALUES ('V6__remove-user-key-aaid');
   INSERT INTO public.installed_migrations VALUES ('V7__count-failed-send-attempts');
   |]

-- This should be an idempotent operation, this means that we should require that the user pass in
-- the ID of the migration that they wish to run up to.
handleMigrationRun :: Maybe MZ.Zone -> AppHandler ()
handleMigrationRun zone = AH.getKeyAndConfirm CONF.rmMigrationKey (handleDatabaseMigrate zone)

-- TODO: For now we just catch everything but in the future we might choose to be more
-- selective...maybe.
exceptionFilter :: E.SomeException -> Maybe E.SomeException
exceptionFilter = Just

simpleCatch :: IO a -> IO (Either E.SomeException a)
simpleCatch = EL.tryJust exceptionFilter

handleDatabaseMigrate :: Maybe MZ.Zone -> AppHandler ()
handleDatabaseMigrate zone = writeError . runExceptT $ do
   target <- ExceptT (m2e missingTarget <$> SC.getParam "target")
   migrationStorePath <- ExceptT $ liftIO (m2e couldNotFindMigrationStore <$> findDirectory addMigrationStorePath)
   options <- withExceptT couldNotParseOptions . ExceptT . getDatabaseOptions $ zone
   let configuration = optionsToConfiguration migrationStorePath options
   potentialError <- liftIO . PGB.withPostgreSQL (databaseConnectionString options) $ \connection -> do
      let parameters = M.makeParameters configuration (Backend.hdbcBackend connection)
      simpleCatch $ M.mainWithParameters ["apply", BSC.unpack target] parameters
   except . A.left migrationRunFailed $ potentialError
   where
      missingTarget = (SH.badRequest, "You must provide the 'target' query parameter as the target to migrate to")
      couldNotParseOptions e = (SH.internalServer, "Could not parse migration options: " <> e)
      couldNotFindMigrationStore = (SH.internalServer, "Could not find the migration store")
      migrationRunFailed e = (SH.internalServer, "Failed to run the database migration: " <> show e)

addMigrationStorePath :: FilePath -> FilePath
addMigrationStorePath f = f </> "dbmigrations"

getDatabaseOptions :: Maybe MZ.Zone -> AppHandler (Either String DatabaseOptions)
getDatabaseOptions Nothing = do
   return . Right $ DatabaseOptions
      { databaseConnectionString = "postgresql://myreminders:myreminders@localhost:5432/myreminders"
      , databaseUser = "myreminders"
      , databasePassword = "myreminders"
      }
getDatabaseOptions _ = do
   pHost     <- siGetEnv  $ DE.pgEnvPre "HOST"
   pPort     <- (readMaybe =<<) <$> siGetEnv (DE.pgEnvPre "PORT") :: AppHandler (Maybe Integer)
   pSchema   <- siGetEnv  $ DE.pgEnvPre "SCHEMA"
   pRole     <- siGetEnv  $ DE.pgEnvPre "ROLE"
   pPassword <- siGetEnv  $ DE.pgEnvPre "PASSWORD"
   case (pHost, pPort, pSchema, pRole, pPassword) of
      (Just host, Just port, Just schema, Just role, Just password) -> do
         let connectionString = "postgresql://" <> role <> ":" <> password <> "@" <> host <> ":" <> show port <> "/" <> schema
         return . Right $ DatabaseOptions
            { databaseConnectionString = connectionString
            , databaseUser = role
            , databasePassword = password
            }
      _ -> return . Left $ "Could not load the database details from the environment variables."
   where
      siGetEnv :: String -> AppHandler (Maybe String)
      siGetEnv = liftIO . DE.getEnv

optionsToConfiguration :: FilePath -> DatabaseOptions -> M.Configuration
optionsToConfiguration migrationStorePath d = M.Configuration
   { M._connectionString = databaseConnectionString d
   , M._migrationStorePath = migrationStorePath
   , M._linearMigrations = False
   , M._timestampFilenames = False
   }

data DatabaseOptions = DatabaseOptions
   { databaseConnectionString      :: String
   , databaseUser                  :: String
   , databasePassword              :: String
   } deriving (Eq)
