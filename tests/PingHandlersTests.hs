module PingHandlersTests where

import Data.ByteString.Char8 as C
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Snap.Test hiding (evalHandler, runHandler)
import Snap.Snaplet.Test

import PingHandlers (handlePings, handleMultiPings)
import Site (app)

prop_trivial :: Bool
prop_trivial = True

tests =
  [ testProperty "trivial" prop_trivial
  ]

prop_postFails = do
  handler <- evalHandler Nothing (postRaw C.empty C.empty C.empty) handlePings app

