{-# LANGUAGE DeriveGeneric #-}
module Connect.Tenant 
   ( ConnectTenant
   , Tenant(..)
   , TenantKey
   ) where

import qualified Connect.AtlassianTypes as CA
import           GHC.Generics
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Network.URI as NU
import Connect.Instances

-- This only knows about the tenant data type but it does not do anything with it
type ConnectTenant = (Tenant, Maybe CA.UserKey)

instance FromJSON Tenant

type TenantKey = T.Text

data Tenant = Tenant 
   { tenantId     :: Integer
   , key          :: TenantKey
   , publicKey    :: T.Text
   , sharedSecret :: T.Text
   , baseUrl      :: ConnectURI
   , productType  :: T.Text
   } deriving (Eq, Show, Generic)
