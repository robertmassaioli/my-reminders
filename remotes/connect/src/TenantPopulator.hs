{-# LANGUAGE OverloadedStrings #-}
module TenantPopulator
  ( getTenantFromTenantKey
  , loadTenant
  , convertTenant
  , toTenant
  ) where

import           Application
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except
import           Data.MaybeUtil
import           SnapHelpers
import qualified Control.Arrow              as A
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified Data.Text.Encoding              as T
import qualified Network.Cryptor            as NC
import qualified Persistence.Tenant         as PT
import qualified Snap.AtlassianConnect      as AC
import qualified Snap.Core                  as SC

getTenantFromTenantKey :: AppHandler (Either (Int, String) AC.Tenant)
getTenantFromTenantKey = runExceptT $ do
    potentialTenantKey <- lift $ SC.getQueryParam "tenantKey"
    tenantKey <- except $ m2e tenantKeyNotFound potentialTenantKey
    ExceptT . loadTenant . T.decodeUtf8 $ tenantKey
    where
        tenantKeyNotFound = (badRequest, "You did not provide a 'tenantKey' on the request.")

loadTenant :: AC.ClientKey -> AppHandler (Either (Int, String) AC.Tenant)
loadTenant clientKey = runExceptT $ do
  eTenant <- ExceptT (m2e noSuchTenant <$> PT.lookupTenant clientKey)
  withExceptT conversionFailed (ExceptT $ convertTenant eTenant)
  where
    noSuchTenant = (notFound, "The tenant could not be found")
    conversionFailed e = (internalServer, "Failed to load tenant details: " <> e)

convertTenant :: PT.EncryptedTenant -> AppHandler (Either String AC.Tenant)
convertTenant t = runExceptT $ do
  sharedSecret <- ExceptT $ (A.left T.unpack <$> NC.decrypt (PT.etEncryptedSharedSecret t) M.empty)
  pure $ toTenant t sharedSecret

toTenant :: PT.EncryptedTenant -> NC.PlainText -> AC.Tenant
toTenant t sharedSecret = AC.Tenant
  { AC.tenantId = PT.etTenantId t
  , AC.key = PT.etKey t
  , AC.publicKey = PT.etPublicKey t
  , AC.oauthClientId = PT.etOauthClientId t
  , AC.sharedSecret = NC.getPlainText sharedSecret
  , AC.baseUrl = PT.etBaseUrl t
  , AC.productType = PT.etProductType t
  }