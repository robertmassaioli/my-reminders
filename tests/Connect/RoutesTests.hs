module Connect.RoutesTests where

import           Connect.Data
import           Connect.Instances
import           Connect.LifecycleResponse
import           Connect.Routes                hiding (getLifecycleResponse)
import           Data.Maybe
import           Data.Text
import           LifecycleHandlers
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
prop_validHostsMakeValidHostNames hostname = property $ validHostName [pack hostname] (getLifecycleResponse hostname)

genHostName :: Gen String
genHostName = listOf1 $ oneof [genUnreserved, genSubDelims]

genUnreserved = elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['-', '.', '_', '~']
genSubDelims = elements ['!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=']
-- genPctEncoded = '%'


getLifecycleResponse:: String -> LifecycleResponse
getLifecycleResponse hostname =
    LifecycleResponseInstalled { lrKey = pack "my-happy-plugin",
                                 lrClientKey = pack "client key",
                                 lrPublicKey = pack "A very public key",
                                 lrSharedSecret = Nothing,
                                 lrServerVersion = Nothing,
                                 lrPluginsVersion = Nothing,
                                 lrBaseUrl = CURI . fromJust . parseURI $ "http://" ++ hostname ++ "/plugin",
                                 lrProductType = Nothing,
                                 lrDescription = Nothing,
                                 lrEventType = Nothing
                               }
