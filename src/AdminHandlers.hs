{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module AdminHandlers
    ( adminSearch
    , adminTenant
    ) where

import           AesonHelpers
import qualified AppConfig              as CONF
import           AppHelpers
import           Application
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           GHC.Generics
import           Persistence.PostgreSQL
import           Persistence.Tenant
import qualified Snap.AtlassianConnect  as AC
import qualified Snap.Core              as SC
import           SnapHelpers
import qualified  Network.URI as NU

data SafeTenant = SafeTenant
    { stKey         :: AC.TenantKey   -- ^ The unique identifier for this tenant accross Atlassian Connect.
    , stPublicKey   :: T.Text      -- ^ The public key for this atlassian connect application.
    , stBaseUrl     :: NU.URI  -- ^ The base url of the Atlassian Cloud host application (product).
    , stProductType :: T.Text      -- ^ The type of product you have connected to in the Atlassian Cloud. (E.g. JIRA, Confluence)
    } deriving (Generic, Eq, Show)

instance ToJSON SafeTenant where
   toJSON = genericToJSON (baseOptions { fieldLabelModifier = stripFieldNamePrefix "st" })

data SafeTenants = SafeTenants
    { stTenants :: [SafeTenant]
    } deriving (Generic, Eq, Show)

instance ToJSON SafeTenants where
   toJSON = genericToJSON (baseOptions { fieldLabelModifier = stripFieldNamePrefix "st" })

toSafeTenants :: [AC.Tenant] -> SafeTenants
toSafeTenants = SafeTenants . fmap toSafeTenant

toSafeTenant :: AC.Tenant -> SafeTenant
toSafeTenant t = SafeTenant
    { stKey = AC.key t
    , stPublicKey = AC.publicKey t
    , stBaseUrl = AC.getURI . AC.baseUrl $ t
    , stProductType = AC.productType t
    }

-- TODO have a GET url where you can get a list of all of the tenants
adminSearch :: AppHandler ()
adminSearch = getKeyAndConfirm CONF.rmAdminKey $ handleMethods
    [ (SC.GET, performTenantSearch)
    ]

performTenantSearch :: AppHandler ()
performTenantSearch = do
    potentialBaseUrl <- SC.getParam "base_url"
    let searchUrl = fromMaybe "" potentialBaseUrl
    tenants <- withConnection (findTenantsByBaseUrl (T.decodeUtf8 searchUrl))
    writeJson . toSafeTenants $ tenants

-- TODO have a DELETE url where you can delete all of the data for a tenant
adminTenant :: AppHandler ()
adminTenant = getKeyAndConfirm CONF.rmAdminKey $ handleMethods
    [ (SC.DELETE, deleteTenant)
    ]

deleteTenant :: AppHandler ()
deleteTenant = do
    potentialTenantKey <- SC.getParam "tenant_key"
    case potentialTenantKey of
        Nothing -> respondWithError badRequest "You need to provide a tenant_key to specify the tenant to delete."
        Just tenantKey -> do
            withConnection (`removeTenantInformation` T.decodeUtf8 tenantKey)
            return ()
