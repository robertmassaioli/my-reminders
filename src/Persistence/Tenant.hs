{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Persistence.Tenant (
    lookupTenant
  , insertTenantInformation
  , removeTenantInformation
  , Tenant(..)
  , TenantKey
  , LifecycleResponse(..)
) where

import qualified Data.Text                            as T
import qualified Data.ByteString.Char8                as B
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ
import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad
import           Data.Aeson.Types
import           Data.Maybe
import           Network.URI hiding                   (query)
import           GHC.Generics
import           Data.Int
import           Connect.Descriptor()

import           Persistence.PostgreSQL

type ClientKey = T.Text

data LifecycleResponse = LifecycleResponseInstalled {
    key'           :: T.Text
  , clientKey'     :: ClientKey
  , publicKey'     :: T.Text
  , sharedSecret'  :: Maybe T.Text
  , serverVersion  :: Maybe T.Text
  , pluginsVersion :: Maybe T.Text
  , baseUrl'       :: URI
  , productType'   :: Maybe T.Text
  , description    :: Maybe T.Text
  , eventType      :: Maybe T.Text
} deriving (Eq, Show, Generic)


instance FromJSON LifecycleResponse where
    parseJSON = genericParseJSON defaultOptions {
                    omitNothingFields = True
                  , fieldLabelModifier = takeWhile (\c -> c /= '_' && c /= '\'')
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
   let newClientKey = clientKey' lri
   let newBaseUri = baseUrl' lri
   oldClientKey <- getClientKeyForBaseUrl conn newBaseUri
   existingTenant <- lookupTenant conn newClientKey
   let newAndOldKeysEqual = fmap (== newClientKey) oldClientKey
   case (existingTenant, newAndOldKeysEqual) of
      -- The base url is already being used by somebody else TODO should warn about this in production
      (_          , Just False) -> return Nothing 
      -- We could not find a tenant with the new key. But the base url found a old client key that matched the new one: error, contradiction
      (Nothing    , Just True)  -> error "This is a contradiction in state, we both could and could not find clientKeys." 
      -- We have never seen this baseUrl and nobody is using that key: brand new tenant, insert
      (Nothing    , Nothing) -> listToMaybe <$> insertTenant conn lri 
      -- We have seen this tenant before but we may have new information for it. Update it.
      (Just tenant, _) -> do
         updateTenantDetails conn newTenant
         return . Just . tenantId $ tenant
         where
            newTenant = tenant
               { publicKey = publicKey' lri 
               , sharedSecret = fromMaybe (sharedSecret tenant) (sharedSecret' lri)
               , baseUrl = baseUrl' lri 
               , productType = fromMaybe (productType tenant) (productType' lri)
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

insertTenant :: Connection -> LifecycleResponse -> IO [Integer]
insertTenant conn lri@(LifecycleResponseInstalled {}) = do
   (fmap join) . liftIO $ insertReturning conn [sql|
      INSERT INTO tenant (key, publicKey, sharedSecret, baseUrl, productType)
      VALUES (?, ?, ?, ?, ?) RETURNING id
   |] (clientKey' lri, publicKey' lri, sharedSecret' lri, show $ baseUrl' lri, productType' lri)

getClientKeyForBaseUrl :: Connection -> URI -> IO (Maybe ClientKey)
getClientKeyForBaseUrl conn baseUrl = do
   clientKeys <- liftIO $ query conn [sql|
      SELECT key from tenant where baseUrl = ?
   |] (Only . show $ baseUrl)
   case clientKeys of
      [] -> return Nothing
      [x] -> return . Just $ x
      _ -> error "There has been a problem in the database model and the baseURl is not unique. Database constraint failure."
