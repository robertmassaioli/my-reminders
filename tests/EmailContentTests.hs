{-# LANGUAGE OverloadedStrings #-}

module EmailContentTests where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error
import Data.Time.Calendar
import Data.Time.Clock
import Mail.Hailgun
import Network.URI
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import EmailContent
import Persistence.Reminder

tests :: [Test]
tests =
  [ testProperty "Email message contains the input message" prop_emailMessageContentContainsMessage
  ]

decode = TE.decodeUtf8With lenientDecode

prop_emailMessageContentContainsMessage :: T.Text -> Property
prop_emailMessageContentContainsMessage tIn =
  tNormalised /= "" ==> counterexample (T.unpack tNormalised ++ "\n" ++ T.unpack messageText) . property $
    tNormalised `T.isInfixOf` messageText
  where
    tNormalised = decode . TE.encodeUtf8 $ tIn
    time = UTCTime (fromGregorian 2000 1 2) (secondsToDiffTime 30)
    uri = fromJust $ parseURI "http://test.com"
    reminder = EmailReminder 1 2 "key" "key2" "stuff" "other stuff" "userkey" "email@address" (Just tNormalised) time uri
    -- This is the actual conversion and extraction:
    messageText = decode $ textContent . reminderEmail $ reminder
