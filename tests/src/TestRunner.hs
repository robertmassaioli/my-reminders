module Main where

import qualified Handler.RestTests
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
                    Handler.RestTests.defaultTestGroup
                ]




