module Connect.Data 
   ( Connect(..)
   , HasConnect(..)
   ) where

import qualified Crypto.Cipher.AES as CCA

data Connect = Connect
   { connectAES :: CCA.AES
   , connectPluginName :: String
   , connectPluginKey :: String
   , connectPageTokenTimeout :: Integer
   }

class HasConnect m where
   getConnect :: m Connect
