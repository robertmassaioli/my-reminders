{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Persistence.Tenant (
    lookupTenant
  , insertTenantInformation
  , removeTenantInformation
  , getTenantCount
  , hibernateTenant
  , wakeTenant
  , markPurgedTenants
  , purgeTenants
  , findTenantsByBaseUrl
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Int
import           Data.Maybe
import qualified Data.Text                        as T
import           Data.Time.Clock                  (UTCTime, getCurrentTime)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           Network.URI                      hiding (query)
import qualified Snap.AtlassianConnect            as AC

import           Persistence.Instances            ()
import           Persistence.PostgreSQL

lookupTenant
   :: Connection
   -> AC.ClientKey
   -> IO (Maybe AC.Tenant)
lookupTenant conn clientKey = do
   -- TODO Can we extract these SQL statements into their own constants so that we can
   -- just re-use them elsewhere?
   tenants <- liftIO $ query conn [sql|
      SELECT id, key, publicKey, sharedSecret, baseUrl, productType
          FROM tenant
          WHERE key = ?
      |]
      (Only clientKey)
   return $ listToMaybe tenants

removeTenantInformation
   :: Connection
   -> AC.ClientKey
   -> IO Int64
removeTenantInformation conn clientKey =
    liftIO $ execute conn [sql| DELETE FROM tenant WHERE key = ?  |] (Only clientKey)

-- There are two interesting cases, you get a client key that matches an existing client key or you
-- get a baseUrl that already exists in the system.
--
-- If you get a client key that matches then you want to perform an update to an existing tenant
-- otherwise you want to register a new tenant
--
-- When updating a tenant, if the baseUrl is different then just update it but you cannot update it
-- to something that already exists in the system.
-- When creating a new tenant then the baseUrl cannot be the same as another one that already exists
-- in the system

insertTenantInformation
   :: Connection
   -> AC.LifecycleResponse
   -> IO (Maybe Integer)
insertTenantInformation conn lri@(AC.LifecycleResponseInstalled {}) = do
   let newClientKey = AC.lrClientKey lri
   let newBaseUri = AC.lrBaseUrl lri
   oldClientKey <- getClientKeyForBaseUrl conn (AC.getURI newBaseUri)
   existingTenant <- lookupTenant conn newClientKey
   let newAndOldKeysEqual = fmap (== newClientKey) oldClientKey
   case (existingTenant, newAndOldKeysEqual) of
      -- The base url is already being used by somebody else TODO should warn about this in production
      (_, Just False) -> return Nothing
      -- We could not find a tenant with the new key. But the base url found a old client key that matched the new one: error, contradiction
      (Nothing, Just True)  -> error "This is a contradiction in state, we both could and could not find clientKeys."
      -- We have never seen this baseUrl and nobody is using that key: brand new tenant, insert
      (Nothing, Nothing) -> listToMaybe <$> rawInsertTenantInformation conn lri
      -- We have seen this tenant before but we may have new information for it. Update it.
      (Just tenant, _) -> do
         updateTenantDetails newTenant conn
         wakeTenant newTenant conn
         return . Just . AC.tenantId $ newTenant
         where
            -- After much discussion it seems that the only thing that we want to update is the base
            -- url if it changes. Everything else should never change unless we delete the tenant
            -- first and then recreate it.
            newTenant = tenant
               { AC.baseUrl = AC.lrBaseUrl lri
               , AC.sharedSecret = fromMaybe (AC.sharedSecret tenant) (AC.lrSharedSecret lri)
               }

updateTenantDetails :: AC.Tenant -> Connection ->  IO Int64
updateTenantDetails tenant conn =
   liftIO $ execute conn [sql|
      UPDATE tenant SET
         publicKey = ?,
         sharedSecret = ?,
         baseUrl = ?,
         productType = ?
      WHERE id = ?
   |] (AC.publicKey tenant, AC.sharedSecret tenant, AC.getURI . AC.baseUrl $ tenant, AC.productType tenant, AC.tenantId tenant)

rawInsertTenantInformation :: Connection -> AC.LifecycleResponse -> IO [Integer]
rawInsertTenantInformation conn lri@(AC.LifecycleResponseInstalled {}) =
   fmap join . liftIO $ insertReturning conn [sql|
      INSERT INTO tenant (key, publicKey, sharedSecret, baseUrl, productType)
      VALUES (?, ?, ?, ?, ?) RETURNING id
   |] (AC.lrClientKey lri, AC.lrPublicKey lri, AC.lrSharedSecret lri, show $ AC.lrBaseUrl lri, AC.lrProductType lri)

getClientKeyForBaseUrl :: Connection -> URI -> IO (Maybe AC.ClientKey)
getClientKeyForBaseUrl conn baseUrl = do
   clientKeys <- liftIO $ query conn [sql|
      SELECT key from tenant where baseUrl = ?
   |] (Only . show $ baseUrl)
   case clientKeys of
      [] -> return Nothing
      [x] -> return . Just $ x
      _ -> error "There has been a problem in the database model and the baseURl is not unique. Database constraint failure."

getTenantCount :: Connection -> IO Int64
getTenantCount conn = do
   counts <- liftIO $ query_ conn [sql|
      SELECT count(*) FROM tenant
   |]
   return . fromOnly . head $ counts

hibernateTenant :: AC.Tenant -> Connection -> IO ()
hibernateTenant tenant conn = do
   currentTime <- getCurrentTime
   liftIO $ execute conn [sql|
      UPDATE tenant SET sleep_date = ? WHERE id = ?
   |] (currentTime, AC.tenantId tenant)
   return ()

wakeTenant :: AC.Tenant -> Connection -> IO ()
wakeTenant tenant conn = do
   liftIO $ execute conn [sql|
      UPDATE tenant SET sleep_date = NULL WHERE id = ?
   |] (Only . AC.tenantId $ tenant)
   return ()

markPurgedTenants :: UTCTime -> Connection -> IO ()
markPurgedTenants beforeTime conn = do
   liftIO $ execute conn [sql|
      INSERT INTO purged_tenant (baseUrl, purgeDate)
      SELECT baseUrl, now() FROM tenant
      WHERE sleep_date IS NOT NULL
      AND sleep_date < ?
   |] (Only beforeTime)
   return ()

purgeTenants :: UTCTime -> Connection -> IO Int64
purgeTenants beforeTime conn = liftIO $ execute conn [sql|
      DELETE FROM tenant WHERE sleep_date IS NOT NULL AND sleep_date < ?
   |] (Only beforeTime)

findTenantsByBaseUrl :: T.Text -> Connection -> IO [AC.Tenant]
findTenantsByBaseUrl uri conn =
   liftIO $ query conn [sql|
      SELECT id, key, publicKey, sharedSecret, baseUrl, productType
      FROM tenant
      WHERE baseUrl like ?
   |] (Only . surroundLike $ uri)

surroundLike :: T.Text -> T.Text
surroundLike x = "%" `T.append` x `T.append` "%"
