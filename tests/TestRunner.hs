module Main where

import Test.Framework

import qualified ReminderHandlersTests
import qualified Connect.RoutesTests
import qualified EmailContentTests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = ReminderHandlersTests.tests
     ++ Connect.RoutesTests.tests
     ++ EmailContentTests.tests
