module EncryptSharedSecret
  ( handleEncryptSharedSecrets
  ) where

import           Application
import qualified Persistence.Tenant as T
import qualified Snap.Core                      as SC
import           SnapHelpers

handleEncryptSharedSecrets :: AppHandler ()
handleEncryptSharedSecrets = handleMethods
  [ (SC.POST, encryptSharedSecrets)
  ]

encryptSharedSecrets :: AppHandler ()
encryptSharedSecrets = do
  result <- T.encryptMoreSharedSecrets
  writeJson result