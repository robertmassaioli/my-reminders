{-# LANGUAGE OverloadedStrings #-}
module Connect.PageToken where

import Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock
import qualified Control.Applicative as CA
import qualified Data.ByteString as DB
import qualified Data.ByteString.Lazy as DBL
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as DT
import qualified Crypto.Cipher.AES as CCA

import qualified Persistence.Tenant as PT

-- TODO We need to add in page token support. This means
-- 1. Being able to create a page token.
-- 1. Being able to verify a page token.

type UserKey = String

data PageToken = PageToken
   { pageTokenHost      :: PT.TenantKey
   , pageTokenUser      :: Maybe UserKey
   , pageTokenTimestamp :: UTCTime
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
         user :: Maybe UserKey -> [Pair]
         user (Just userKey)  = [ "u" .= userKey ]
         user Nothing         = []

         polling :: Bool -> [Pair]
         polling True   = [ "p" .= DT.pack "1" ] -- TODO this is stupid, should put a javascript true out
         polling False  = []

instance FromJSON PageToken where
   parseJSON (Object tokenData) = PageToken
      CA.<$> tokenData .: "h" 
      CA.<*> tokenData .:? "u"
      CA.<*> tokenData .: "t"
      CA.<*> tokenData .:? "p" .!= False
   parseJSON _ = fail "The PageToken should contain a JSON object."

generateToken :: PT.Tenant -> Maybe UserKey -> UTCTime -> PageToken
generateToken tenant userKey timestamp = PageToken
   { pageTokenHost = PT.key tenant
   , pageTokenUser = userKey
   , pageTokenTimestamp = timestamp
   , pageTokenAllowInsecurePolling = False
   }

-- TODO in order to write the token out:
-- 1. Generate the token.
-- 1. Write the token out to json in the supported format.
-- 1. Base64 encode the json.
-- 1. AES ECB encrypt the string
encryptPageToken :: CCA.AES -> PageToken -> DB.ByteString
encryptPageToken aes pageToken = encryptedToken
   where
      tokenAsJson = encode pageToken
      tokenAsBase64 = B64.encode . DBL.toStrict $ tokenAsJson
      encryptedToken = CCA.encryptECB aes tokenAsBase64

decryptPageToken :: CCA.AES -> DB.ByteString -> Either String PageToken
decryptPageToken aes input = B64.decode decryptedToken >>= eitherDecode . DBL.fromStrict
   where
      decryptedToken = CCA.decryptECB aes input

-- TODO we should write unit tests to ensure that every combination of this encrypt and decrypt
-- can be translated correctly.
