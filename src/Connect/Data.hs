module Connect.Data
  ( Connect(..)
  , HasConnect(..)
  ) where

import Connect.Descriptor
import qualified Crypto.Cipher.AES as CCA

data Connect = Connect
  { connectAES :: CCA.AES
  , connectPluginName :: Name Connect
  , connectPluginKey :: PluginKey
  , connectPageTokenTimeout :: Timeout
  }

class HasConnect m where
  getConnect :: m Connect
