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

-- TODO move this into its own module as it is the same for both installed and uninstalled
data LifecycleResponse = LifecycleResponseInstalled {
    key'           :: T.Text
  , clientKey'     :: T.Text
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

instance FromRow Tenant where
    fromRow = Tenant <$> field <*> field <*> field <*> field <*> field <*> field

instance FromField URI where
    fromField _ (Just bstr) = pure $ fromMaybe nullURI $ parseURI (B.unpack bstr)
    fromField f _           = returnError ConversionFailed f "data is not a valid URI value"

instance ToField URI where
    toField = Escape . B.pack . show

lookupTenant 
   :: Connection
   -> T.Text
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
   -> T.Text
   -> IO Int64
removeTenantInformation conn clientKey =
    liftIO $ execute conn [sql| DELETE FROM tenant WHERE key = ?  |] (Only clientKey)

insertTenantInformation 
   :: Connection
   -> LifecycleResponse
   -> IO (Maybe Integer)
insertTenantInformation conn LifecycleResponseInstalled{..} = do
        liftIO $ execute conn [sql|
                DELETE FROM tenant WHERE key = ?
            |] (Only clientKey')
        tenantId' <- liftIO $ insertReturning conn [sql|
            INSERT INTO tenant (key, publicKey, sharedSecret, baseUrl, productType)
                VALUES (?, ?, ?, ?, ?) RETURNING id
            |] (clientKey', publicKey', sharedSecret', show baseUrl', productType')
        return (listToMaybe (join tenantId'))

getTenantCount :: Connection -> IO Int64
getTenantCount conn = do
   counts <- liftIO $ query_ conn [sql|
      SELECT count(*) FROM tenant
   |]
   return . fromOnly . head $ counts
