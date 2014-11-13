{-# LANGUAGE OverloadedStrings #-}

module EmailContentTests where

import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error
import Data.Time.Clock
import Data.Time.Calendar
import Data.Maybe (fromJust)
import Mail.Hailgun
import Network.URI
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, PropertyM)

import EmailContent
import Persistence.Reminder

tests :: [Test]
tests =
  [ testProperty "Email message contains the input message" prop_emailMessageContentContainsMessage
  ]

decode = TE.decodeUtf8With strictDecode

prop_emailMessageContentContainsMessage :: String -> Property
prop_emailMessageContentContainsMessage s =
  s' /= "" ==> counterexample (T.unpack messageText) . ioProperty $ do
    putStrLn s'
    return . property $ remainder /= messageText
  where
    s' = filter isPrint s
    time = UTCTime (fromGregorian 2000 1 2) (secondsToDiffTime 30)
    uri = fromJust $ parseURI "http://test.com"
    packed = T.pack s'
    reminder = EmailReminder 1 2 "key" "key2" "stuff" "other stuff" "userkey" "email@address" (Just packed) time uri
    -- This is the actual conversion and extraction:
    messageText = decode $ textContent . reminderEmail $ reminder
    (_, remainder) = T.breakOn packed messageText