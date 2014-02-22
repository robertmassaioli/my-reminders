module Main where

import qualified Handler.RestTests
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
                    Handler.RestTests.defaultTestGroup
                ]




