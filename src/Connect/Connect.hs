{-# LANGUAGE OverloadedStrings #-}

module Connect.Connect
  ( Connect(..)
  , initConnectSnaplet
  ) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Configurator as DC
import qualified Data.Configurator.Types as DCT
import Data.Text
import qualified Control.Monad as CM
import qualified Control.Monad.IO.Class as MI
import qualified Crypto.Cipher.AES as CCA
import qualified Connect.PageToken as PT
--import           Network.URI
import qualified Snap.Snaplet as SS
import qualified System.Exit as SE

import           Connect.Data
import           Connect.Descriptor
import           ConfigurationHelpers

-- This should be the Atlassian Connect Snaplet
-- The primary purpose of this Snaplet should be to load Atlassian Connect specific configuration.
-- We should be able to start a snaplet that actually provides no routes but gives a bunch of
-- required configuration information back to the person that needs it. Actually, it would be really
-- great if we could provide our own Atlassian Connect specific routes in here. That way this
-- connect snaplet could be responsible for providing the Atlassian Connect plugin json descriptor
-- to the rest of the plugin. That would be great!

toConnect :: ConnectConfig -> Connect
toConnect conf = Connect
  { connectAES = CCA.initAES $ ccSecretKey conf
  , connectPluginName = Name $ ccPluginName conf
  , connectPluginKey = PluginKey $ ccPluginKey conf
  , connectPageTokenTimeout = Timeout $ ccPageTokenTimeout conf
  --, connectBaseUrl = ccBaseUrl conf
  }

initConnectSnaplet :: SS.SnapletInit b Connect
initConnectSnaplet = SS.makeSnaplet "Connect" "Atlassian Connect state and operations." (Just configDataDir) $
  MI.liftIO $ CM.liftM toConnect $ SS.loadAppConfig "connect.cfg" "resources" >>= loadConnectConfig

data ConnectConfig = ConnectConfig
  { ccSecretKey :: BSC.ByteString
  , ccPluginName :: Text
  , ccPluginKey :: Text
  , ccPageTokenTimeout :: Integer
  --, ccBaseUrl :: URI
  }

loadConnectConfig :: DCT.Config -> IO ConnectConfig
loadConnectConfig connectConf = do
  --baseUrl <- require connectConf "plugin_base_url" "Missing the plugin base url in the configuration file."
  name <- require connectConf "plugin_name" "Missing plugin name in connect configuration file."
  key <- require connectConf "plugin_key" "Missing plugin key in connect configuration file."
  secret <- require connectConf "secret_key" "Missing secret key in connect configuration file."
  let keyLength = BSC.length secret
  CM.when (keyLength /= 32) $ do
    putStrLn $ "Expected Atlassian Connect secret_key to be 32 Hex Digits long but was actually: " ++ show keyLength
    SE.exitWith (SE.ExitFailure 1)
  pageTokenTimeoutInSeconds <- DC.lookupDefault PT.defaultTimeoutSeconds connectConf "page_token_timeout_seconds"
  return ConnectConfig
    { ccPluginName = name
    , ccPluginKey = key
    , ccSecretKey = secret
    , ccPageTokenTimeout = pageTokenTimeoutInSeconds
    --, ccBaseUrl = baseUrl
    }

{-
instance DCT.Configured URI where
   convert (DCT.String s) = parseURI . unpack $ s
   convert _              = Nothing
-}
