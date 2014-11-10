module MigrationHandler 
   (migrationRequest
   ) where

import           Application
import           Control.Applicative ((<$>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (filterM)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.EnvironmentHelpers as DE
import           Data.Maybe (listToMaybe)
import           Data.List (inits)
import qualified AppConfig as CONF
import qualified Snap.Core                as SC
import qualified SnapHelpers as SH
import qualified System.Directory as SD
import           System.Environment (getExecutablePath)
import           System.FilePath (dropFileName, (</>), splitDirectories, joinPath)
import           System.Process (callProcess)
import           Text.Read (readMaybe)

migrationRequest :: AppHandler ()
migrationRequest = SH.handleMethods
   [ (SC.PUT, handleMigrationRun)
   ]

-- This should be an idempotent operation, this means that we should require that the user pass in
-- the ID of the migration that they wish to run up to.
handleMigrationRun :: AppHandler ()
handleMigrationRun = SH.getKeyAndConfirm CONF.rmMigrationKey handleFlywayMigrate

handleFlywayMigrate :: AppHandler ()
handleFlywayMigrate = either (SH.respondWithError SH.badRequest) (liftIO . flywayMigrate) =<< getFlywayOptions

flywayMigrate :: FlywayOptions -> IO ()
flywayMigrate options = do
   potentialFlywayPath <- findFlywayExecutable
   case potentialFlywayPath of
      Just flywayPath -> callProcess flywayPath migrationArguments
      Nothing -> fail "Could not find the flyway executable relative to the running executables path."
   where
      migrationArguments = "migrate" : flywayOptionsToArguments options
   
getFlywayOptions :: AppHandler (Either String FlywayOptions)
getFlywayOptions = do
   potentialTarget <- (readMaybe . BSC.unpack =<<) <$> SC.getParam (BSC.pack "target")
   case potentialTarget of
      Nothing -> return . Left $ "You need to provide a 'target' schema version param to the migration endpoint."
      Just target -> do
         pHost     <- siGetEnv  $ pgRemindMePre "HOST"
         pPort     <- (readMaybe =<<) <$> siGetEnv (pgRemindMePre "PORT") :: AppHandler (Maybe Integer)
         pSchema   <- siGetEnv  $ pgRemindMePre "SCHEMA"
         pRole     <- siGetEnv  $ pgRemindMePre "ROLE"
         pPassword <- siGetEnv  $ pgRemindMePre "PASSWORD"
         case (pHost, pPort, pSchema, pRole, pPassword) of
            (Just host, Just port, Just schema, Just role, Just password) -> do
               let connectionString = "jdbc:postgresql://" ++ host ++ ":" ++ show port ++ "/" ++ schema
               return . Right $ FlywayOptions
                  { flywayUrl = connectionString
                  , flywayUser = role
                  , flywayPassword = password
                  , flywayTarget = target
                  }
            _ -> return . Left $ "Could not load the database details from the environment variables."
   where
      siGetEnv :: String -> AppHandler (Maybe String)
      siGetEnv = liftIO . DE.getEnv
   
      pgRemindMePre :: String -> String
      pgRemindMePre = (++) "PG_REMIND_ME_"

findFlywayExecutable :: IO (Maybe FilePath)
findFlywayExecutable = do
   exeDir <- getExecutableDirectory
   let potentialLocations = fmap addFlywayPath (getParentDirectories exeDir)
   fmap listToMaybe $ filterM SD.doesFileExist potentialLocations

getExecutableDirectory :: IO FilePath
getExecutableDirectory = fmap dropFileName getExecutablePath

getParentDirectories :: FilePath -> [FilePath]
getParentDirectories = reverse . fmap joinPath . tail . inits . splitDirectories

addFlywayPath :: FilePath -> FilePath
addFlywayPath f = f </> "migrations" </> "flyway"

data FlywayOptions = FlywayOptions
   { flywayUrl :: String
   , flywayUser :: String
   , flywayPassword :: String
   , flywayTarget :: Integer
   } deriving (Eq)

flywayOptionsToArguments :: FlywayOptions -> [String]
flywayOptionsToArguments fo = 
   [ "-url=" ++ flywayUrl fo
   , "-user=" ++ flywayUser fo
   , "-password=" ++ flywayPassword fo
   , "-target=" ++ (show . flywayTarget $ fo)
   ]
