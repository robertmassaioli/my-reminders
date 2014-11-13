{-# LANGUAGE OverloadedStrings #-}

module EmailContentTests where

import Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock
import Data.Time.Calendar
import Data.Maybe (fromJust)
import Mail.Hailgun
import Network.URI
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import EmailContent
import Persistence.Reminder

tests :: [Test]
tests =
  [ testProperty "Email message contains the input message" prop_emailMessageContentContainsMessage
  ]

prop_emailMessageContentContainsMessage :: String -> Property
prop_emailMessageContentContainsMessage s =
  s /= "" ==>
  counterexample (T.unpack messageText) . property $
    remainder /= messageText
  where
    time = UTCTime (fromGregorian 2000 1 2) (secondsToDiffTime 30)
    uri = fromJust $ parseURI "http://test.com"
    packed = T.pack s
    reminder = EmailReminder 1 2 "key" "key2" "stuff" "other stuff" "userkey" "email@address" (Just packed) time uri
    -- This is the actual conversion and extraction:
    messageText = TE.decodeUtf8 $ textContent . reminderEmail $ reminder
    (_, remainder) = T.breakOn packed messageText