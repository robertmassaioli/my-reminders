module Connect.Tenant 
   ( ConnectTenant
   ) where

import qualified Persistence.Tenant as PT
import qualified Connect.AtlassianTypes as CA

type ConnectTenant = (PT.Tenant, Maybe CA.UserKey)

