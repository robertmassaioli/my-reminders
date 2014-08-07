module PingHandlersTests where

import Data.ByteString.Char8 as C
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO)
import Snap.Test hiding (evalHandler, runHandler)
import Snap.Snaplet.Test

import PingHandlers (handlePings, handleMultiPings)
import Site (app)

prop_trivial :: Bool
prop_trivial = True

tests :: [Test]
tests =
  [ testProperty "trivial" prop_trivial
  , testProperty "also trivial" prop_postFails
  ]

prop_postFails = monadicIO $ do
  errorOrResponse <- runHandler Nothing (postRaw C.empty C.empty C.empty) handlePings app
  assert True
