module Connect.PageToken where

import Data.Aeson
import qualified Data.ByteString.Base64 as B64
import qualified Crypto.Cipher.AES as CCA

import qualified Persistence.Tenant as PT

-- TODO We need to add in page token support. This means
-- 1. Being able to create a page token.
-- 1. Being able to verify a page token.

type UserKey = String

data PageToken = PageToken
   { pageTokenHost      :: String
   , pageTokenUser      :: Maybe UserKey
   , pageTokenTimestamp :: Date
   , pageTokenAllowInsecurePolling :: Bool
   }
   deriving (Eq, Show)

instance DA.ToJSON PageToken where
   toJSON (PageToken host user timestamp insecurePolling) = DA.object
      [ "h" .= host
      , "t" .= timestamp
      ] ++ user ++ polling
      where
         user :: Maybe UserKey -> [Pair]
         user (Just userKey)  = [ "u" .= userKey ]
         user Nothing         = []

         polling :: Bool -> [Pair]
         polling True   = [ "p" .= "1" ] -- TODO this is stupid, should put a javascript true out
         polling False  = []

instance DA.FromJSON PageToken where
   parseJSON (Object tokenData) = PageToken
      <$> tokenData .: "h" 
      <*> tokenData .:? "u"
      <*> tokenData .: "t"
      <*> tokenData .:? "p" .:= False
   parseJSON _ = fail "The PageToken should contain a JSON object."

generateToken :: PT.Tenant -> Maybe UserKey -> Date -> PageToken
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
encryptPageToken :: CCA.AES -> PageToken -> ByteString
encryptPageToken aes pageToken = encryptedToken
   where
      tokenAsJson = encode pageToken
      tokenAsBase64 = B64.encode tokenAsJson
      encryptedToken = CCA.encryptECB aes tokenAsBase64

decryptPageToken :: CCA.AES -> ByteString -> Either String PageToken
decryptPageToken decryptedToken input = B64.decode decryptedToken >>= decode
   where
      decryptedToken = CCA.decryptECB aes input

-- TODO we should write unit tests to ensure that every combination of this encrypt and decrypt
-- can be translated correctly.
