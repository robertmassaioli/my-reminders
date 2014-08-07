module TestHelpers where

import Data.ByteString.Char8 as C
import Data.Text
import Network.URI
import Snap.Core
import Test.QuickCheck

import SnapHelpers

responseCodeIs :: Int -> Either Text Response -> Bool
responseCodeIs code errorOrRsp = either (\_ -> False) ((== code) . rspStatus) errorOrRsp

instance Arbitrary C.ByteString where
  arbitrary = fmap C.pack arbitrary
