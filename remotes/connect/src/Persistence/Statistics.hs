{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Persistence.Statistics
    ( Statistics(..)
    , getStatistics
    ) where

import           Application
import           Data.Aeson
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Generics
import           Snap.Snaplet.PostgresqlSimple

data Statistics = Statistics
    { liveTenants               :: Integer
    , purgePendingTenants       :: Integer
    , reminderCount             :: Integer
    , minRemindersPerEmail      :: Maybe Integer
    , avgRemindersPerEmail      :: Maybe Double
    , stddevRemindersPerEmail   :: Maybe Double
    , maxRemindersPerEmail      :: Maybe Integer
    , minRemindersPerTenant     :: Maybe Integer
    , averageRemindersPerTenant :: Maybe Double
    , stddevRemindersPerTenant  :: Maybe Double
    , maxRemindersPerTenant     :: Maybe Integer
    , countAfterExpiry             :: Integer
    , countAfterRetirement         :: Integer
    } deriving (Show, Generic)

instance FromRow Statistics where
  fromRow = Statistics <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToJSON Statistics

getStatistics :: AppHandler Statistics
getStatistics = do
    [result] <- query_
        [sql|
            select
            	(select count(*) from tenant where sleep_date is null) as live_tenants,
            	(select count(*) from tenant where sleep_date is not null) as purge_pending_tenants,
            	(select count(*) from reminder) as reminder_count,
            	min(remcount.email_count) as min_reminders_per_email,
            	(avg(remcount.email_count) :: double precision) as avg_reminders_per_email,
            	(stddev_pop(remcount.email_count) :: double precision) as stddev_reminders_per_email,
            	max(remcount.email_count) as max_reminders_per_email,
            	min(tencount.tenant_count) as min_reminders_per_tenant,
            	(avg(tencount.tenant_count) :: double precision) as avg_reminders_per_tenant,
            	(stddev_pop(tencount.tenant_count) :: double precision) as stddev_reminders_per_tenant,
                max(tencount.tenant_count) as max_reminders_per_tenant,
                (SELECT count(*) FROM reminder WHERE EXTRACT(EPOCH FROM date) > 1709251200) as count_after_expiry,
                (SELECT count(*) FROM reminder WHERE EXTRACT(EPOCH FROM date) > 1717164000) as count_after_retirement
            FROM
            	(select count(tenantid) as tenant_count, tenantid as tid from reminder group by tenantid) as tencount,
            	(select count(userAaid) as email_count from reminder group by userAaid) as remcount;
        |]
    return result
