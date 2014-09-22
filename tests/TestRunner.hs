module Main where

import Test.Framework

import qualified PingHandlersTests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = PingHandlersTests.tests
