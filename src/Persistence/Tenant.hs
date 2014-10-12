{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Persistence.Tenant (
    lookupTenant
  , insertTenantInformation
  , removeTenantInformation
  , getTenantCount
  , Tenant(..)
  , TenantKey
  , LifecycleResponse(..)
) where

import           AesonHelpers
import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad
import qualified Data.Text                            as T
import qualified Data.ByteString.Char8                as B
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ
import           Data.Aeson.Types
import           Data.Maybe
import           Network.URI hiding                   (query)
import           GHC.Generics
import           Data.Int
import           Connect.Descriptor()

import           Persistence.PostgreSQL

type ClientKey = T.Text

data LifecycleResponse = LifecycleResponseInstalled {
    lrKey           :: T.Text
  , lrClientKey     :: ClientKey
  , lrPublicKey     :: T.Text
  , lrSharedSecret  :: Maybe T.Text
  , lrServerVersion  :: Maybe T.Text
  , lrPluginsVersion :: Maybe T.Text
  , lrBaseUrl       :: URI
  , lrProductType   :: Maybe T.Text
  , lrDescription    :: Maybe T.Text
  , lrEventType      :: Maybe T.Text
} deriving (Eq, Show, Generic)


instance FromJSON LifecycleResponse where
    parseJSON = genericParseJSON defaultOptions 
      { omitNothingFields = True
      , fieldLabelModifier = stripFieldNamePrefix "lr"
      }

instance FromJSON Tenant

type TenantKey = T.Text

data Tenant = Tenant {
    tenantId     :: Integer
  , key          :: TenantKey
  , publicKey    :: T.Text
  , sharedSecret :: T.Text
  , baseUrl      :: URI 
  , productType  :: T.Text
} deriving (Eq, Show, Generic)

instance FromRow ClientKey where
   fromRow = field

instance FromRow Tenant where
    fromRow = Tenant <$> field <*> field <*> field <*> field <*> field <*> field

instance FromField URI where
    fromField _ (Just bstr) = pure $ fromMaybe nullURI $ parseURI (B.unpack bstr)
    fromField f _           = returnError ConversionFailed f "data is not a valid URI value"

instance ToField URI where
    toField = Escape . B.pack . show

lookupTenant 
   :: Connection
   -> ClientKey
   -> IO (Maybe Tenant)
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
   -> ClientKey
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
   -> LifecycleResponse
   -> IO (Maybe Integer)
insertTenantInformation conn lri@(LifecycleResponseInstalled {}) = do
   let newClientKey = lrClientKey lri
   let newBaseUri = lrBaseUrl lri
   oldClientKey <- getClientKeyForBaseUrl conn newBaseUri
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
         updateTenantDetails conn newTenant
         return . Just . tenantId $ tenant
         where
            -- After much discussion it seems that the only thing that we want to update is the base
            -- url if it changes. Everything else should never change unless we delete the tenant
            -- first and then recreate it.
            newTenant = tenant
               { baseUrl = lrBaseUrl lri 
               , sharedSecret = fromMaybe (sharedSecret tenant) (lrSharedSecret lri)
               }

updateTenantDetails :: Connection -> Tenant -> IO Int64
updateTenantDetails conn tenant = do
   liftIO $ execute conn [sql|
      UPDATE tenant SET 
         publicKey = ?,
         sharedSecret = ?,
         baseUrl = ?,
         productType = ?
      WHERE id = ?
   |] (publicKey tenant, sharedSecret tenant, baseUrl tenant, productType tenant, tenantId tenant)

rawInsertTenantInformation :: Connection -> LifecycleResponse -> IO [Integer]
rawInsertTenantInformation conn lri@(LifecycleResponseInstalled {}) = do
   (fmap join) . liftIO $ insertReturning conn [sql|
      INSERT INTO tenant (key, publicKey, sharedSecret, baseUrl, productType)
      VALUES (?, ?, ?, ?, ?) RETURNING id
   |] (lrClientKey lri, lrPublicKey lri, lrSharedSecret lri, show $ lrBaseUrl lri, lrProductType lri)

getClientKeyForBaseUrl :: Connection -> URI -> IO (Maybe ClientKey)
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
