module TestHelpers where

import Data.ByteString.Char8 as C
import Data.CaseInsensitive (CI)
import Data.Text as T
import Network.URI
import Snap.Core
import Test.QuickCheck
import Test.Framework (TestOptions)

import SnapHelpers
import WithToken

import Debug.Trace (trace)

testTrace :: Show a => String -> a -> a
testTrace input x = trace (input ++ ": " ++ show x) x

responseCodeIs :: Int -> Either Text Response -> Property
responseCodeIs code errorOrRsp = either
  (\t -> counterexample (T.unpack t) False)
  (property . (== code) . testTrace "Response Status" . rspStatus)
  errorOrRsp

emptyPath :: C.ByteString
emptyPath = C.empty

emptyPageTokenHeader :: (CI C.ByteString, C.ByteString)
emptyPageTokenHeader = (acHeaderName, C.empty)

emptyTestOptions = mempty :: TestOptions
