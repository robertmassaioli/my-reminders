{-# LANGUAGE DeriveGeneric             #-}

module Healthcheck
   ( healthcheckRequest
   ) where

import           Application
import           AesonHelpers (baseOptions, stripFieldNamePrefix)
import           Control.Applicative ((<$>))
import           Control.Concurrent.ParallelIO.Local (parallel, withPool)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text as T
import           Data.Time.Clock
import           GHC.Generics
import qualified Snap.Core as SC
import           SnapHelpers

healthcheckRequest :: AppHandler ()
healthcheckRequest = handleMethods
   [ ( SC.GET, getHealthcheckRequest )
   ]

getHealthcheckRequest :: AppHandler ()
getHealthcheckRequest = do
   runResult <- liftIO $ runHealthchecks curatedHealthchecks
   if anyHealthcheckFailed runResult
   then do
      writeJson runResult
      respondWith serviceUnavaliable
   else
      respondNoContent
   -- Run the healthchecks in IO collate the results and return them

curatedHealthchecks :: [Healthcheck]
curatedHealthchecks = 
   [ failCheck
   ]

runHealthchecks :: [Healthcheck] -> IO HealthcheckRunResult
runHealthchecks healthchecks = HealthcheckRunResult <$> withPool 2 (flip parallel healthchecks)

anyHealthcheckFailed :: HealthcheckRunResult -> Bool
anyHealthcheckFailed (HealthcheckRunResult statuses) = any (not . hsIsHealthy) statuses

failCheck :: Healthcheck
failCheck = do
   ct <- getCurrentTime
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

type Healthcheck = IO HealthStatus

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
