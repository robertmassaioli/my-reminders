{-# LANGUAGE OverloadedStrings #-}
module Connect.PageToken
  ( PageToken(..) -- TODO make it so that you can query the token but nothing else
  , generateToken
  , generateTokenCurrentTime
  , encryptPageToken
  , decryptPageToken
  , defaultTimeoutSeconds
  ) where

import qualified Control.Applicative    as CA
import qualified Crypto.Cipher.AES      as CCA
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString        as DB
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy   as DBL
import qualified Data.Padding           as DP
import qualified Data.Text              as DT
import           Data.Time.Clock
import           Data.Time.Units

import qualified Connect.AtlassianTypes as CA
import qualified Connect.Tenant         as CT

defaultTimeoutSeconds :: Second
defaultTimeoutSeconds = 5 * 60

data PageToken = PageToken
  { pageTokenHost                 :: CT.TenantKey
  , pageTokenUser                 :: Maybe CA.UserKey
  , pageTokenTimestamp            :: UTCTime
  , pageTokenAllowInsecurePolling :: Bool
  }
  deriving (Eq, Show)

instance ToJSON PageToken where
  toJSON (PageToken host potentialUser timestamp insecurePolling) = object $
    [ "h" .= host
    , "t" .= timestamp
    ]
    ++ user potentialUser
    ++ polling insecurePolling
    where
      user :: Maybe CA.UserKey -> [Pair]
      user (Just userKey)  = [ "u" .= userKey ]
      user Nothing      = []

      polling :: Bool -> [Pair]
      polling True  = [ "p" .= DT.pack "1" ] -- Unfortunately we need to write out a true
      polling False  = []

instance FromJSON PageToken where
  parseJSON (Object tokenData) = PageToken
    CA.<$> tokenData .: "h"
    CA.<*> tokenData .:? "u"
    CA.<*> tokenData .: "t"
    CA.<*> tokenData .:? "p" .!= False
  parseJSON _ = fail "The PageToken should contain a JSON object."

generateToken :: CT.ConnectTenant -> UTCTime -> PageToken
generateToken (tenant, userKey) timestamp = PageToken
  { pageTokenHost = CT.key tenant
  , pageTokenUser = userKey
  , pageTokenTimestamp = timestamp
  , pageTokenAllowInsecurePolling = False
  }

generateTokenCurrentTime :: CT.ConnectTenant -> IO PageToken
generateTokenCurrentTime ct = fmap (generateToken ct) getCurrentTime

-- In order to write the token out:
-- 1. Generate the token.
-- 1. Write the token out to json in the supported format.
-- 1. Base64 encode the json.
-- 1. AES ECB encrypt the string
encryptPageToken :: CCA.AES -> PageToken -> DB.ByteString
encryptPageToken aes pageToken = encryptedEncodedToken
  where
    tokenAsJson = encode pageToken
    tokenAsBase64 = B64.encode . DBL.toStrict $ tokenAsJson
    paddedBase64 = DP.zeroPad 16 tokenAsBase64
    encryptedToken = CCA.encryptECB aes paddedBase64
    encryptedEncodedToken = B64.encode encryptedToken

decryptPageToken :: CCA.AES -> DB.ByteString -> Either String PageToken
decryptPageToken aes input = do
  undecodedEncryptedToken <- B64.decode input
  let decryptedToken = CCA.decryptECB aes undecodedEncryptedToken
  (B64.decode . DP.zeroUnpad $ decryptedToken) >>= eitherDecode . DBL.fromStrict

-- TODO we should write unit tests to ensure that every combination of this encrypt and decrypt
-- can be translated correctly.
