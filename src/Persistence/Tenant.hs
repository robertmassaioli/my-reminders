{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric     #-}

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
  , encryptMoreSharedSecrets
  , EncryptResult(..)
) where

import           Application
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Function                    (on)
import           Data.Int
import           Data.Maybe
import           Data.Time.Clock                  (UTCTime, getCurrentTime)
import           Database.PostgreSQL.Simple.SqlQQ
import           Network.URI                      hiding (query)
import           Persistence.Instances            ()
import qualified Snap.AtlassianConnect            as AC
import           Snap.Snaplet.PostgresqlSimple
import qualified Network.Cryptor                  as NC
import qualified Data.Map                         as M
import qualified Data.Text                        as T
import           GHC.Generics
import           Data.Aeson
import           AesonHelpers

data EncryptResult = EncryptResult
   { erErrors :: [T.Text]
   , erUpdated :: Int
   } deriving (Show, Generic)

instance ToJSON EncryptResult where
   toJSON = genericToJSON (baseOptions { fieldLabelModifier = stripFieldNamePrefix "er" })

encryptMoreSharedSecrets :: AppHandler EncryptResult
encryptMoreSharedSecrets = withTransaction $ do
   unencryptedTenants <- query_
      [sql|
      SELECT id, key, publicKey, oauthClientId, sharedSecret, baseUrl, productType
      FROM tenant
      WHERE encrypted_shared_secret IS NULL
      LIMIT 30
      |]
   tenantsWithEncryption <- forM unencryptedTenants $ \ut -> do
      result <- NC.encrypt (NC.PlainText . AC.sharedSecret $ ut) M.empty
      return (ut, result)
   let (errors, converts) = partitionEithers tenantsWithEncryption
   forM converts $ \(ut, cipherText) -> do
      execute
         [sql|
         UPDATE tenant SET encrypted_shared_secret = ?
         WHERE id = ?
         |] (cipherText, AC.tenantId ut)
   return $ EncryptResult
      { erErrors = fmap snd errors
      , erUpdated = length converts
      }

partitionEithers :: [(a, Either b c)] -> ([(a, b)], [(a, c)])
partitionEithers = foldl update ([], [])

update :: ([(a, b)], [(a, c)]) -> (a, Either b c) -> ([(a, b)], [(a, c)])
update (xx, yy) (x, Left l) = ((x, l):xx, yy)
update (xx, yy) (x, Right r) = (xx, (x, r):yy)

lookupTenant
   :: AC.ClientKey
   -> AppHandler (Maybe AC.Tenant)
lookupTenant clientKey = do
   -- TODO Can we extract these SQL statements into their own constants so that we can
   -- just re-use them elsewhere?
   tenants <- query [sql|
      SELECT id, key, publicKey, oauthClientId, sharedSecret, baseUrl, productType
          FROM tenant
          WHERE key = ?
      |]
      (Only clientKey)
   return $ listToMaybe tenants

removeTenantInformation
   :: AC.ClientKey
   -> AppHandler Int64
removeTenantInformation clientKey =
   execute [sql| DELETE FROM tenant WHERE key = ?  |] (Only clientKey)

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
   :: Maybe AC.Tenant
   -> AC.LifecycleResponse
   -> AppHandler (Maybe Integer)
insertTenantInformation maybeTenant lri@(AC.LifecycleResponseInstalled {}) = do
   let newClientKey = AC.lrClientKey lri
   let newBaseUri = AC.lrBaseUrl lri
   oldClientKey <- getClientKeyForBaseUrl (AC.getURI newBaseUri)
   existingTenant <- lookupTenant newClientKey
   let newAndOldKeysEqual = fmap (== newClientKey) oldClientKey
   case (existingTenant, newAndOldKeysEqual, maybeTenant) of
      -- The base url is already being used by somebody else TODO should warn about this in production
      (_, Just False, _) -> return Nothing
      -- We could not find a tenant with the new key. But the base url found a old client key that matched the new one: error, contradiction
      (Nothing, Just True, _)  -> error "This is a contradiction in state, we both could and could not find clientKeys."
      -- We have never seen this baseUrl and nobody is using that key: brand new tenant, insert
      (Nothing, Nothing, _) -> listToMaybe <$> rawInsertTenantInformation lri
      -- We have seen this tenant before but we may have new information for it. Update it.
      (Just tenant, _, Just claimedTenant) | on (==) AC.key tenant claimedTenant -> do
         updateTenantDetails newTenant
         wakeTenant newTenant
         return . Just . AC.tenantId $ newTenant
         where
            -- After much discussion it seems that the only thing that we want to update is the base
            -- url if it changes. Everything else should never change unless we delete the tenant
            -- first and then recreate it.
            newTenant = tenant
               { AC.baseUrl = AC.lrBaseUrl lri
               , AC.sharedSecret = fromMaybe (AC.sharedSecret tenant) (AC.lrSharedSecret lri)
               }
      -- The authorisation between tenants does not match
      (_, _, _) -> return Nothing

updateTenantDetails :: AC.Tenant ->  AppHandler Int64
updateTenantDetails tenant =
   execute [sql|
      UPDATE tenant SET
         publicKey = ?,
         sharedSecret = ?,
         baseUrl = ?,
         productType = ?
      WHERE id = ?
   |] (AC.publicKey tenant, AC.sharedSecret tenant, AC.getURI . AC.baseUrl $ tenant, AC.productType tenant, AC.tenantId tenant)

rawInsertTenantInformation :: AC.LifecycleResponse -> AppHandler [Integer]
rawInsertTenantInformation lri@(AC.LifecycleResponseInstalled {}) =
   fmap join $ returning [sql|
      INSERT INTO tenant (key, publicKey, sharedSecret, baseUrl, productType)
      VALUES (?, ?, ?, ?, ?) RETURNING id
   |] [(AC.lrClientKey lri, AC.lrPublicKey lri, AC.lrSharedSecret lri, show $ AC.lrBaseUrl lri, AC.lrProductType lri)]

getClientKeyForBaseUrl :: URI -> AppHandler (Maybe AC.ClientKey)
getClientKeyForBaseUrl baseUrl = do
   clientKeys <- query [sql|
      SELECT key from tenant where baseUrl = ?
   |] (Only . show $ baseUrl)
   case clientKeys of
      [] -> return Nothing
      [x] -> return . Just $ x
      _ -> error "There has been a problem in the database model and the baseURl is not unique. Database constraint failure."

getTenantCount :: AppHandler Int64
getTenantCount = do
   counts <- query_ [sql|
      SELECT count(*) FROM tenant
   |]
   return . fromOnly . head $ counts

hibernateTenant :: AC.Tenant -> AppHandler ()
hibernateTenant tenant = do
   currentTime <- liftIO $ getCurrentTime
   execute [sql|
      UPDATE tenant SET sleep_date = ? WHERE id = ?
   |] (currentTime, AC.tenantId tenant)
   return ()

wakeTenant :: AC.Tenant -> AppHandler ()
wakeTenant tenant = do
   execute [sql|
      UPDATE tenant SET sleep_date = NULL WHERE id = ?
   |] (Only . AC.tenantId $ tenant)
   return ()

markPurgedTenants :: UTCTime -> AppHandler ()
markPurgedTenants beforeTime = do
   execute [sql|
      INSERT INTO purged_tenant (baseUrl, purgeDate)
      SELECT baseUrl, now() FROM tenant
      WHERE sleep_date IS NOT NULL
      AND sleep_date < ?
   |] (Only beforeTime)
   return ()

purgeTenants :: UTCTime -> AppHandler Int64
purgeTenants beforeTime = execute [sql|
      DELETE FROM tenant WHERE sleep_date IS NOT NULL AND sleep_date < ?
   |] (Only beforeTime)

findTenantsByBaseUrl :: T.Text -> AppHandler [AC.Tenant]
findTenantsByBaseUrl uri =
   query [sql|
      SELECT id, key, publicKey, oauthClientId, sharedSecret, baseUrl, productType
      FROM tenant
      WHERE baseUrl like ?
   |] (Only . surroundLike $ uri)

surroundLike :: T.Text -> T.Text
surroundLike x = "%" `T.append` x `T.append` "%"
