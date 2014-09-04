module Connect.RoutesTests where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Snap.Snaplet.Test

tests :: [Test]
tests =
  [ testProperty "hosts construct with domains from Connect are all considered valid" prop_validHostsMakeValidHostNames
  ]

prop_validHostsMakeValidHostNames :: Property
prop_validHostsMakeValidHostNames = property False

genHostName :: Gen String
genHostName = listOf1 $ oneof [genUnreserved, genSubDelims]

genUnreserved = elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['-', '.', '_', '~']
genSubDelims = elements ['!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=']
-- genPctEncoded = '%'
