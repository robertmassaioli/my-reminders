{-# LANGUAGE DeriveGeneric             #-}

module Healthcheck
   ( healthcheckRequest
   ) where

import           Application
import           AesonHelpers (baseOptions, stripFieldNamePrefix)
import           Control.Applicative ((<$>))
import           Control.Concurrent.ParallelIO.Local (parallel, withPool)
import qualified Control.Exception as E
import           Control.Monad.CatchIO (tryJust)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe (isNothing)
import qualified Data.Text as T
import           Data.Time.Clock
import           Database.PostgreSQL.Simple (SqlError(..))
import           GHC.Generics
import           Mail.Hailgun (getDomains, herMessage, Page(..))
import           Persistence.PostgreSQL (withConnection)
import           Persistence.Tenant (getTenantCount)
import qualified RemindMeConfiguration as RC
import qualified Snap.Core as SC
import           SnapHelpers

healthcheckRequest :: AppHandler ()
healthcheckRequest = handleMethods
   [ ( SC.GET, getHealthcheckRequest )
   ]

getHealthcheckRequest :: AppHandler ()
getHealthcheckRequest = do
   runResult <- runHealthchecks curatedHealthchecks
   if anyHealthcheckFailed runResult
   then do
      writeJson runResult
      respondWith serviceUnavaliable
   else
      respondNoContent

-- Important: This is our curated list of healthchecks. Anything in this list will run as a
-- healthcheck for this service.
curatedHealthchecks :: [Healthcheck]
curatedHealthchecks = 
   [ databaseHealthCheck
   , mailgunHealthcheck
   , failCheck
   ]

runHealthchecks :: [Healthcheck] -> AppHandler HealthcheckRunResult
runHealthchecks healthchecks = HealthcheckRunResult <$> sequence healthchecks

anyHealthcheckFailed :: HealthcheckRunResult -> Bool
anyHealthcheckFailed (HealthcheckRunResult statuses) = any (not . hsIsHealthy) statuses

databaseHealthCheck :: Healthcheck
databaseHealthCheck = do
   currentTime <- liftIO getCurrentTime
   result <- tryJust exceptionFilter (withConnection getTenantCount)
   return $ status (either Just (const Nothing) result) currentTime
   where
      status :: E.Exception e => Maybe e -> UTCTime -> HealthStatus
      status potentialException currentTime = HealthStatus
         { hsName = T.pack "Database Connection Check"
         , hsDescription = T.pack "Ensures that this service can connect to the PostgreSQL Relational Database that contains all of the Reminders."
         , hsIsHealthy = isNothing $ potentialException
         , hsFailureReason = do
             exception <- potentialException
             return . T.pack $ "Could not connect to the remote database. Addon will not work correctly. Message: " ++ show exception
         , hsApplication = remindMeApplication
         , hsTime = currentTime
         , hsSeverity = CRITICAL
         , hsDocumentation = Just . T.pack $ "If you see this error you might want to check out the database and see "
            ++ "what is going on there. And then ensure that the application has been passed the correct database credentials."
         }

exceptionFilter :: E.SomeException -> Maybe E.SomeException
exceptionFilter e = Just e

mailgunHealthcheck :: Healthcheck
mailgunHealthcheck = do
   currentTime <- liftIO getCurrentTime
   hailgunContext <- fmap RC.rmHailgunContext RC.getRMConf
   result <- tryJust exceptionFilter (liftIO $ getDomains hailgunContext smallPage)
   return $ status (either (Just . show) (either (Just . herMessage) (const Nothing)) result) currentTime
   where
      status :: Maybe String -> UTCTime -> HealthStatus
      status potentialException currentTime = HealthStatus
         { hsName = T.pack "Mailgun Connection Check"
         , hsDescription = T.pack "Ensures that this service can connect to the the Mailgun service so that we can send reminder emails successfully."
         , hsIsHealthy = isNothing $ potentialException
         , hsFailureReason = do
             exception <- potentialException
             return . T.pack $ "Could not connect to the Mailgun service. Addon will not work correctly. Message: " ++ show exception
         , hsApplication = remindMeApplication
         , hsTime = currentTime
         , hsSeverity = CRITICAL
         , hsDocumentation = Just . T.pack $ "If you see this error you might want to check out the Mailgun status page to see if they are having problems: http://status.mailgun.com/. "
            ++ "Otherwise you should check that the mailgun credentials are correct and, if they are, then contact Mailgun support for help."
         }

      smallPage = Page 0 3

remindMeApplication :: HealthcheckApplication
remindMeApplication = ConnectApplication { haName = T.pack "remind-me-connect" }

failCheck :: Healthcheck
failCheck = do
   ct <- liftIO getCurrentTime
   return $ HealthStatus
      { hsName = T.pack "Failing healthcheck"
      , hsDescription = T.pack "I always fail...that's how this healthcheck rolls."
      , hsIsHealthy = False
      , hsFailureReason = Just . T.pack $ "I always fail. Read the description"
      , hsApplication = ConnectApplication { haName = T.pack "remind-me-connect" }
      , hsTime = ct
      , hsSeverity = UNDEFINED
      , hsDocumentation = Nothing
      }

type Healthcheck = AppHandler HealthStatus

data HealthcheckRunResult = HealthcheckRunResult
   { hrrStatus :: [HealthStatus]
   }

instance ToJSON HealthcheckRunResult where
   toJSON hrr@(HealthcheckRunResult {}) = object [ (T.pack "status") .= hrrStatus hrr ]

data HealthStatus = HealthStatus
   { hsName             :: T.Text
   , hsDescription      :: T.Text
   , hsIsHealthy        :: Bool
   , hsFailureReason    :: Maybe T.Text
   , hsApplication      :: HealthcheckApplication
   , hsTime             :: UTCTime
   , hsSeverity         :: HealthStatusSeverity
   , hsDocumentation    :: Maybe T.Text
   } deriving (Generic)

instance ToJSON HealthStatus where
   toJSON = genericToJSON (baseOptions { fieldLabelModifier = stripFieldNamePrefix "hs" })

data HealthcheckApplication = ConnectApplication 
   { haName :: T.Text
   }

instance ToJSON HealthcheckApplication where
   toJSON = toJSON . haName

data HealthStatusSeverity 
   = UNDEFINED
   | MINOR
   | MAJOR
   | WARNING
   | CRITICAL
   deriving(Eq, Ord, Show, Generic)

instance ToJSON HealthStatusSeverity

{-
 - Example healthcheck response

{
    "status": [
        {
            "name": "Add-on Group Health Check",
            "description": "This was provided by plugin 'com.atlassian.plugins.atlassian-connect-plugin:addonsGroupHealthCheck' via class 'com.atlassian.plugin.connect.healthcheck.AtlassianAddonsGroupHealthCheck'",
            "isHealthy": true,
            "failureReason": "",
            "application": "Plugin",
            "time": 1412117623809,
            "severity": "UNDEFINED",
            "documentation": ""
        },
        ...
    ]
}

-}
