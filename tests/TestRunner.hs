module Main where

import Test.Framework

import qualified PingHandlersTests
import qualified Connect.RoutesTests as RoutesTests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = PingHandlersTests.tests ++ RoutesTests.tests
