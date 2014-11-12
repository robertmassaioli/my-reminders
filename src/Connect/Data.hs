module Connect.Data
  ( Connect(..)
  , HasConnect(..)
  , HostName
  ) where

import           Connect.Descriptor
import qualified Crypto.Cipher.AES  as CCA
import qualified Data.Text          as T
import qualified Network.URI        as NU

data Connect = Connect
  { connectAES              :: CCA.AES
  , connectPluginName       :: Name Connect
  , connectPluginKey        :: PluginKey
  , connectBaseUrl          :: NU.URI
  , connectPageTokenTimeout :: Timeout
  , connectHostWhitelist    :: [HostName]
  }

type HostName = T.Text

class HasConnect m where
  getConnect :: m Connect
