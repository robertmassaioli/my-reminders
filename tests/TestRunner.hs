module Main where

import Test.Framework

import qualified ReminderHandlersTests as RHT
import qualified Connect.RoutesTests as RoutesTests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = RHT.tests ++ RoutesTests.tests
