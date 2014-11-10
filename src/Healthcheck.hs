{-# LANGUAGE DeriveGeneric             #-}

module Healthcheck
   ( healthcheckRequest
   ) where

import           Application
import           AesonHelpers (baseOptions, stripFieldNamePrefix)
import           Control.Applicative ((<$>))
import qualified Control.Exception as E
import           Control.Monad.CatchIO (MonadCatchIO(..), tryJust)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe (isNothing)
import qualified Data.Text as T
import           Data.Time.Clock
import qualified Data.Time.Units as DTU
import           Data.TimeUnitUTC
import           GHC.Generics
import           Mail.Hailgun (getDomains, herMessage, Page(..))
import           Persistence.PostgreSQL (withConnection)
import           Persistence.Tenant (getTenantCount)
import           Persistence.Reminder (getExpiredReminders)
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
   , expiryHealthcheck
   --, failCheck
   ]

runHealthchecks :: [Healthcheck] -> AppHandler HealthcheckRunResult
runHealthchecks healthchecks = HealthcheckRunResult <$> sequence healthchecks

anyHealthcheckFailed :: HealthcheckRunResult -> Bool
anyHealthcheckFailed (HealthcheckRunResult statuses) = any (not . hsIsHealthy) statuses

simpleCatch :: (MonadCatchIO m, Functor m) => m a -> m (Either E.SomeException a)
simpleCatch = tryJust exceptionFilter

databaseHealthCheck :: Healthcheck
databaseHealthCheck = do
   currentTime <- liftIO getCurrentTime
   result <- simpleCatch (withConnection getTenantCount)
   return $ status (either Just (const Nothing) result) currentTime
   where
      status :: E.Exception e => Maybe e -> UTCTime -> HealthStatus
      status potentialException currentTime = HealthStatus
         { hsName = T.pack "Database Connection Check"
         , hsDescription = T.pack "Ensures that this service can connect to the PostgreSQL Relational Database that contains all of the Reminders."
         , hsIsHealthy = isNothing potentialException
         , hsFailureReason = do
             exception <- potentialException
             return . T.pack $ "Could not connect to the remote database. Addon will not work correctly. Message: " ++ show exception
         , hsApplication = application
         , hsTime = currentTime
         , hsSeverity = CRITICAL
         , hsDocumentation = Just . T.pack $ "If you see this error you might want to check out the database and see "
            ++ "what is going on there. And then ensure that the application has been passed the correct database credentials."
         }

mailgunHealthcheck :: Healthcheck
mailgunHealthcheck = do
   currentTime <- liftIO getCurrentTime
   hailgunContext <- fmap RC.rmHailgunContext RC.getRMConf
   result <- simpleCatch (liftIO $ getDomains hailgunContext smallPage)
   return $ status (either (Just . show) (either (Just . herMessage) (const Nothing)) result) currentTime
   where
      status :: Maybe String -> UTCTime -> HealthStatus
      status potentialException currentTime = HealthStatus
         { hsName = T.pack "Mailgun Connection Check"
         , hsDescription = T.pack "Ensures that this service can connect to the the Mailgun service so that we can send reminder emails successfully."
         , hsIsHealthy = isNothing potentialException
         , hsFailureReason = do
             exception <- potentialException
             return . T.pack $ "Could not connect to the Mailgun service. Addon will not work correctly. Message: " ++ show exception
         , hsApplication = application
         , hsTime = currentTime
         , hsSeverity = CRITICAL
         , hsDocumentation = Just . T.pack $ "If you see this error you might want to check out the Mailgun status page to see if they are having problems: http://status.mailgun.com/. "
            ++ "Otherwise you should check that the mailgun credentials are correct and, if they are, then contact Mailgun support for help."
         }

      smallPage = Page 0 3

-- The purpose of this Healthcheck is to ensure that the third party that is supposed to be
-- triggering expiry is actually doing its job.
expiryHealthcheck :: Healthcheck 
expiryHealthcheck = do
   currentTime <- liftIO getCurrentTime
   expiryWindowMaxMinutes <- fmap RC.rmMaxExpiryWindowMinutes RC.getRMConf
   let expireTime = addUTCTime (negate $ timeUnitToDiffTime expiryWindowMaxMinutes) currentTime
   result <- simpleCatch (withConnection $ getExpiredReminders expireTime)
   return $ case result of
      Left e -> status (Just . show $ e) currentTime expiryWindowMaxMinutes
      Right reminders -> 
         if null reminders
            then status Nothing currentTime expiryWindowMaxMinutes
            else status (Just $ "The number of reminders outside the window is: " ++ (show . length $ reminders)) currentTime expiryWindowMaxMinutes
   where 
      status :: (DTU.TimeUnit a, Show a) => Maybe String -> UTCTime -> a -> HealthStatus
      status potentialException currentTime windowSize = HealthStatus
         { hsName = T.pack "Expiry Window Exceeded Healthcheck"
         , hsDescription = T.pack $ "Ensures that reminders are sent in a timely manner and not outside a window of size: " ++ show windowSize
         , hsIsHealthy = isNothing potentialException
         , hsFailureReason = do
             exception <- potentialException
             return . T.pack $ "Reminders are not being sent in a timely manner. Addon is not working correctly. Message: " ++ show exception
         , hsApplication = application
         , hsTime = currentTime
         , hsSeverity = CRITICAL
         , hsDocumentation = Just . T.pack $ "If you see this error start by making sure that the database healthcheck succeeds. If it does then "
            ++ "check that the service that triggers the expiry handler has been firing correctly. Likely to be Easy Cron (http://www.easycron.com)."
            ++ "After that contact the developers for further aid."
         }
         

application :: HealthcheckApplication
application = ConnectApplication { haName = T.pack "my-reminders" }

{-
-- This Healthcheck remains for testing purposes
failCheck :: Healthcheck
failCheck = do
   ct <- liftIO getCurrentTime
   return $ HealthStatus
      { hsName = T.pack "Failing healthcheck"
      , hsDescription = T.pack "I always fail...that's how this healthcheck rolls."
      , hsIsHealthy = False
      , hsFailureReason = Just . T.pack $ "I always fail. Read the description"
      , hsApplication = application
      , hsTime = ct
      , hsSeverity = UNDEFINED
      , hsDocumentation = Nothing
      }
-}


-- TODO: For now we just catch everything but in the future we might choose to be more
-- selective...maybe.
exceptionFilter :: E.SomeException -> Maybe E.SomeException
exceptionFilter = Just

type Healthcheck = AppHandler HealthStatus

data HealthcheckRunResult = HealthcheckRunResult
   { hrrStatus :: [HealthStatus]
   }

instance ToJSON HealthcheckRunResult where
   toJSON hrr@(HealthcheckRunResult {}) = object [ T.pack "status" .= hrrStatus hrr ]

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
