{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Cryptor
  ( PlainText(..)
  , CipherText(..)
  , EncryptionContext
  , initCryptorSnaplet
  , HasCryptor(..)
  , CryptorConf
  , encrypt
  , decrypt
  , isHealthy
  ) where

import           Data.Aeson
import           GHC.Generics
import qualified MicrosZone                 as MZ
import qualified Snap.Snaplet               as SS
import qualified Data.Text                  as T
import qualified Data.Map                   as M
import qualified Network.HTTP.Client        as NC
import qualified Network.Api.Support        as NS
import qualified Data.ByteString.Lazy       as BSL
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           AesonHelpers
import           Network.HTTP.Types.Method  (StdMethod(GET, POST))
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.FromField

newtype PlainText = PlainText { getPlainText :: T.Text } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToField, FromField)
newtype CipherText = CipherText { getCipherText :: T.Text } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToField, FromField)

type EncryptionContext = M.Map T.Text T.Text

data EncryptionRequest = EncryptionRequest
  { ereqPlainText :: PlainText
  , ereqEncryptionContext :: EncryptionContext
  } deriving (Show, Generic)

instance ToJSON EncryptionRequest where
  toJSON = genericToJSON (baseOptions { fieldLabelModifier = stripFieldNamePrefix "ereq" })

data EncryptionResponse = EncryptionResponse
  { erspCipherText :: CipherText
  } deriving (Show, Generic)

instance FromJSON EncryptionResponse where
  parseJSON = genericParseJSON baseOptions { fieldLabelModifier = stripFieldNamePrefix "ersp" }

data DecryptionRequest = DecryptionRequest
  { dreqCipherText :: CipherText
  , dreqEncryptionContext :: EncryptionContext
  } deriving (Show, Generic)

instance ToJSON DecryptionRequest where
  toJSON = genericToJSON (baseOptions { fieldLabelModifier = stripFieldNamePrefix "dreq" })

data DecryptionResponse = DecryptionResponse
  { drspPlainText :: PlainText
  } deriving (Show, Generic)

instance FromJSON DecryptionResponse where
  parseJSON = genericParseJSON baseOptions { fieldLabelModifier = stripFieldNamePrefix "drsp" }

data CryptorConf = CryptorConf
  { cMicrosZone :: Maybe MZ.Zone
  } deriving (Show)

class HasCryptor m where
  getCryptor :: m CryptorConf

initCryptorSnaplet :: Maybe MZ.Zone -> SS.SnapletInit b CryptorConf
initCryptorSnaplet zone = SS.makeSnaplet "Cryptor" "Communicate with cryptor in a secure manner" Nothing $ do
  return $ CryptorConf zone

standardCryptor :: NS.RequestTransformer
standardCryptor =
  NS.addHeader ("Content-Type", "application/json; charset=utf-8") <>
  NS.addHeader ("X-Cryptor-Client", "my-reminders")

responder :: FromJSON a => Int -> BSL.ByteString -> NS.JsonResult a
responder 200 body = NS.parseBody body
responder _ _ = NS.ParseError "Did not get back a 200 response code."

encrypt :: (Monad m, MonadIO m, HasCryptor m) => PlainText -> EncryptionContext -> m (Either T.Text CipherText)
encrypt plainText context = do
  cryptorConf <- getCryptor
  case (cMicrosZone cryptorConf) of
    Nothing -> return . Right . CipherText . getPlainText $ plainText
    Just _ -> do
      response <- liftIO $ NS.runRequest NC.defaultManagerSettings POST encryptUrl requestDetails (NS.basicResponder responder)
      case response of
        NS.JsonSuccess x -> return . Right . erspCipherText $ x
        NS.ParseError x -> return . Left $ "Parse error: " <> x
        NS.DecodeError x -> return . Left $ "Decode error: " <> x
  where
    encryptUrl = "http://cryptor-sidecar:26272/cryptor/encrypt/micros/my-reminders/secret-data"
    requestDetails =
      standardCryptor <>
      NS.setJson payload
    payload = EncryptionRequest
      { ereqPlainText = plainText
      , ereqEncryptionContext = context
      }


decrypt :: (Monad m, MonadIO m, HasCryptor m) => CipherText -> EncryptionContext -> m (Either T.Text PlainText)
decrypt cipherText context = do
  cryptorConf <- getCryptor
  case (cMicrosZone cryptorConf) of
    Nothing -> return . Right . PlainText . getCipherText $ cipherText
    Just _ -> do
      response <- liftIO $ NS.runRequest NC.defaultManagerSettings POST decryptUrl requestDetails (NS.basicResponder responder)
      case response of
        NS.JsonSuccess x -> return . Right . drspPlainText $ x
        NS.ParseError x -> return . Left $ "Parse error: " <> x
        NS.DecodeError x -> return . Left $ "Decode error: " <> x
  where
    decryptUrl = "http://cryptor-sidecar:26272/cryptor/decrypt/micros/my-reminders/secret-data"
    requestDetails =
      standardCryptor <>
      NS.setJson payload
    payload = DecryptionRequest
      { dreqCipherText = cipherText
      , dreqEncryptionContext = context
      }

isHealthy :: (Monad m, MonadIO m, HasCryptor m) => m Bool
isHealthy = do
  cryptorConf <- getCryptor
  case (cMicrosZone cryptorConf) of
    Nothing -> return True
    Just _ -> liftIO $ NS.runRequest NC.defaultManagerSettings GET healthcheckUrl standardCryptor (NS.basicResponder successResponder)
  where
    healthcheckUrl = "http://cryptor-sidecar:26272/healthcheck"

    successResponder 200 _ = True
    successResponder 204 _ = True
    successResponder _ _ = False