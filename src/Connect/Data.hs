module Connect.Data
  ( Connect(..)
  , HasConnect(..)
  , HostName
  , DynamicDescriptorConfig(..)
  ) where

-- Connect Modules
import           Connect.Descriptor

-- Standard Modules
import qualified Crypto.Cipher.AES  as CCA
import qualified Data.Text          as T
import qualified Network.URI        as NU

data Connect = Connect
  { connectAES              :: CCA.AES
  , connectPlugin           :: Plugin
  , connectPluginName       :: Name Connect
  , connectPluginKey        :: PluginKey
  , connectBaseUrl          :: NU.URI
  , connectPageTokenTimeout :: Timeout
  , connectHostWhitelist    :: [HostName]
  }

type HostName = T.Text

class HasConnect m where
  getConnect :: m Connect

data DynamicDescriptorConfig = DynamicDescriptorConfig
  { dcPluginName :: Name Plugin
  , dcPluginKey  :: PluginKey
  , dcBaseUrl    :: NU.URI
  }