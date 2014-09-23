module Connect.RoutesTests where

import           Connect.Data
import           Connect.Routes
import           Data.Maybe
import           Data.Text
import           Network.URI                          (URI (..), parseURI)
import           Persistence.Tenant
import           Snap.Snaplet.Test
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck


tests :: [Test]
tests =
  [ testProperty "hosts construct with domains from Connect are all considered valid" (forAll genHostName prop_validHostsMakeValidHostNames)
  ]

prop_validHostsMakeValidHostNames :: String -> Property
prop_validHostsMakeValidHostNames hostname = property $ (validHostName [pack hostname] (getLifecycleResponse hostname)) == True

genHostName :: Gen String
genHostName = listOf1 $ oneof [genUnreserved, genSubDelims]

genUnreserved = elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['-', '.', '_', '~']
genSubDelims = elements ['!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=']
-- genPctEncoded = '%'


getLifecycleResponse:: String -> LifecycleResponse
getLifecycleResponse hostname =
    LifecycleResponseInstalled { key' = pack "my-happy-plugin",
                                 clientKey' = pack "client key",
                                 publicKey' = pack "A very public key",
                                 sharedSecret' = Nothing,
                                 serverVersion = Nothing,
                                 pluginsVersion = Nothing,
                                 baseUrl' = fromJust $ parseURI ("http://" ++ hostname ++ "/plugin"),
                                 productType' = Nothing,
                                 description = Nothing,
                                 eventType = Nothing
                               }
