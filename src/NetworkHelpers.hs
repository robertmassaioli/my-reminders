module NetworkHelpers 
   ( setProxy
   , setPotentialProxy
   , getProxyFromConf
   ) where

import           Data.List (isPrefixOf)
import           Data.Monoid (Endo(..))
import           Network.Api.Support
import           Network.HTTP.Client (Proxy(..))
import           Network.HTTP.Client.Internal (addProxy)
import           RemindMeConfiguration

setProxy :: Proxy -> RequestTransformer
setProxy (Proxy pHost pPort) = Endo $ addProxy pHost pPort

maybeEndo :: (a -> Endo b) -> Maybe a -> Endo b
maybeEndo = maybe (Endo id)

setPotentialProxy :: Maybe Proxy -> RequestTransformer
setPotentialProxy = maybeEndo setProxy

getProxyFromConf :: String -> RMConf -> Maybe Proxy
getProxyFromConf baseUrl conf = 
  if isPrefixOf "https" baseUrl
    then rmHttpSecureProxy conf
    else rmHttpProxy conf
