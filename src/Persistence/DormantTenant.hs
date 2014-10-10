{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

module Persistence.DormantTenant
   ( DormantTenant(..)
   , hibernateTenant
   , wakeTenant
   , markPurgedTenants
   , purgeTenants
   ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Int (Int64)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Generics
import qualified Persistence.Tenant as PT

data DormantTenant = DormantTenant
   { dormantTenantId :: Integer
   , dormantTenantSleepDate :: UTCTime
   } deriving (Eq, Show, Generic)

hibernateTenant :: PT.Tenant -> Connection -> IO DormantTenant
hibernateTenant tenant conn = do
   dt <- createDormantTenant tenant
   liftIO $ execute conn [sql|
      INSERT INTO dormant_tenant (tenantId, sleepDate) VALUES (?, ?)
   |] (dormantTenantId dt, dormantTenantSleepDate dt)
   return dt

wakeTenant :: PT.Tenant -> Connection -> IO ()
wakeTenant tenant conn = do
   liftIO $ execute conn [sql|
      DELETE FROM dormant_tenant where tenantId = ?
   |] (Only . PT.tenantId $ tenant)
   return ()

markPurgedTenants :: UTCTime -> Connection -> IO ()
markPurgedTenants beforeTime conn = do
   liftIO $ execute conn [sql|
      INSERT INTO purged_tenant (baseUrl, purgeDate)
      SELECT t.baseUrl, now() FROM tenant t, dormant_tenant dt
      WHERE t.id = dt.tenantId
      AND dt.sleepDate < ?
   |] (Only beforeTime)
   return ()

purgeTenants :: UTCTime -> Connection -> IO Int64
purgeTenants beforeTime conn = liftIO $ execute conn [sql|
      DELETE FROM tenant WHERE id IN (SELECT tenantId from dormant_tenant WHERE sleepDate < ?)
   |] (Only beforeTime)

createDormantTenant :: PT.Tenant -> IO DormantTenant
createDormantTenant tenant = do
   currentTime <- getCurrentTime
   return DormantTenant
      { dormantTenantId = PT.tenantId tenant
      , dormantTenantSleepDate = currentTime
      }
