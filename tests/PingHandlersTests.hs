module PingHandlersTests where

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import PingHandlers (handlePings, handleMultiPings)

prop_trivial :: Bool
prop_trivial = True

tests =
  [ testProperty "trivial" prop_trivial
  ]
