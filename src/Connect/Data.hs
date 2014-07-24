module Connect.Data
  ( Connect(..)
  , HasConnect(..)
  ) where

import qualified Crypto.Cipher.AES as CCA
import Data.Text

data Connect = Connect
  { connectAES :: CCA.AES
  , connectPluginName :: Text
  , connectPluginKey :: Text
  , connectPageTokenTimeout :: Integer
  }

class HasConnect m where
  getConnect :: m Connect
