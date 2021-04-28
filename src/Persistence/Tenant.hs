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
  , EncryptedTenant(..)
) where

import           AesonHelpers
import           Application
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Either.Combinators          (rightToMaybe)
import           Data.Int
import           Data.Maybe
import           Data.Time.Clock                  (UTCTime, getCurrentTime)
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Generics
import           Network.URI                      hiding (query)
import           Persistence.Instances            ()
import           Snap.Snaplet.PostgresqlSimple
import qualified Data.Map                         as M
import qualified Data.Text                        as T
import qualified Network.Cryptor                  as NC
import qualified Snap.AtlassianConnect            as AC

data EncryptedTenant = EncryptedTenant
   { etTenantId               :: Integer       -- ^ Your identifier for this tenant.
   , etKey                    :: AC.TenantKey     -- ^ The unique identifier for this tenant accross Atlassian Connect.
   , etPublicKey              :: T.Text        -- ^ The public key for this atlassian connect application.
   , etOauthClientId          :: Maybe T.Text  -- ^ The OAuth Client Id for this tenant. If this add-on does not support user impersonation then this may not be present.
   , etEncryptedSharedSecret  :: NC.CipherText -- ^ The encrypted shared secret for this atlassian connect application. Used for JWT token generation.
   , etBaseUrl                :: AC.ConnectURI -- ^ The base url of the Atlassian Cloud host application (product).
   , etProductType            :: T.Text        -- ^ The type of product you have connected to in the Atlassian Cloud. (E.g. JIRA, Confluence)
   } deriving (Eq, Show, Generic)

instance FromRow EncryptedTenant where
   fromRow = EncryptedTenant <$> field <*> field <*> field <*> field <*> field <*> (AC.CURI <$> field) <*> field

lookupTenant
   :: AC.ClientKey
   -> AppHandler (Maybe EncryptedTenant)
lookupTenant clientKey = do
   -- TODO Can we extract these SQL statements into their own constants so that we can
   -- just re-use them elsewhere?
   tenants <- query [sql|
      SELECT id, key, publicKey, oauthClientId, encrypted_shared_secret, baseUrl, productType
          FROM tenant
          WHERE key = ?
      |]
      (Only clientKey)
   return $ listToMaybe tenants

removeTenantInformation :: AC.ClientKey -> AppHandler Int64
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
      (Nothing, Nothing, _) -> rawInsertTenantInformation lri
      -- We have seen this tenant before but we may have new information for it. Update it.
      (Just tenant, _, Just claimedTenant) | etKey tenant == AC.key claimedTenant -> do
         encryptedSharedSecret <- encryptSharedSecret lri
         let newTenant = genNewTenant encryptedSharedSecret
         updateTenantDetails newTenant
         wakeTenant newTenant
         return . Just . etTenantId $ newTenant
         where
            -- After much discussion it seems that the only thing that we want to update is the base
            -- url if it changes. Everything else should never change unless we delete the tenant
            -- first and then recreate it.
            genNewTenant potentialEncryptedSecret = tenant
               { etBaseUrl = AC.lrBaseUrl lri
               , etEncryptedSharedSecret = fromMaybe (etEncryptedSharedSecret tenant) (rightToMaybe potentialEncryptedSecret)
               }
      -- The authorisation between tenants does not match
      (_, _, _) -> return Nothing

updateTenantDetails :: EncryptedTenant ->  AppHandler Int64
updateTenantDetails tenant =
   execute [sql|
      UPDATE tenant SET
         publicKey = ?,
         encrypted_shared_secret = ?,
         baseUrl = ?,
         productType = ?
      WHERE id = ?
   |] (etPublicKey tenant, etEncryptedSharedSecret tenant, AC.getURI . etBaseUrl $ tenant, etProductType tenant, etTenantId tenant)

encryptSharedSecret :: AC.LifecycleResponse -> AppHandler (Either T.Text NC.CipherText)
encryptSharedSecret lri = case AC.lrSharedSecret lri of
   Nothing -> return . Left $ "No shared secret found in lifecycle payload"
   Just sharedSecret -> NC.encrypt (NC.PlainText sharedSecret) M.empty

rawInsertTenantInformation :: AC.LifecycleResponse -> AppHandler (Maybe Integer)
rawInsertTenantInformation lri@(AC.LifecycleResponseInstalled {}) = do
   pEncryptedSharedSecret <- encryptSharedSecret lri
   case pEncryptedSharedSecret of
      Left _ -> return Nothing
      Right encryptedSharedSecret -> listToMaybe . fmap fromOnly <$> query
         [sql|
            INSERT INTO tenant (key, publicKey, encrypted_shared_secret, baseUrl, productType)
            VALUES (?, ?, ?, ?, ?) RETURNING id
         |] (AC.lrClientKey lri, AC.lrPublicKey lri, encryptedSharedSecret, show $ AC.lrBaseUrl lri, AC.lrProductType lri)

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

hibernateTenant :: EncryptedTenant -> AppHandler ()
hibernateTenant tenant = do
   currentTime <- liftIO $ getCurrentTime
   execute [sql|
      UPDATE tenant SET sleep_date = ? WHERE id = ?
   |] (currentTime, etTenantId tenant)
   return ()

wakeTenant :: EncryptedTenant -> AppHandler ()
wakeTenant tenant = do
   execute [sql|
      UPDATE tenant SET sleep_date = NULL WHERE id = ?
   |] (Only . etTenantId $ tenant)
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

findTenantsByBaseUrl :: T.Text -> AppHandler [EncryptedTenant]
findTenantsByBaseUrl uri =
   query [sql|
      SELECT id, key, publicKey, oauthClientId, encrypted_shared_secret, baseUrl, productType
      FROM tenant
      WHERE baseUrl like ?
   |] (Only . surroundLike $ uri)

surroundLike :: T.Text -> T.Text
surroundLike x = "%" `T.append` x `T.append` "%"
