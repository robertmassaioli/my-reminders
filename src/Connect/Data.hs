module Connect.Data
  ( Connect(..)
  , HasConnect(..)
  , HostName
  ) where

import           Connect.Descriptor
import qualified Crypto.Cipher.AES  as CCA
import qualified Data.Text          as T

data Connect = Connect
  { connectAES              :: CCA.AES
  , connectPluginName       :: Name Connect
  , connectPluginKey        :: PluginKey
  , connectPageTokenTimeout :: Timeout
  , connectHostWhitelist    :: [HostName]
  }

type HostName = T.Text

class HasConnect m where
  getConnect :: m Connect
