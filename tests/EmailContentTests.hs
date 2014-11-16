{-# LANGUAGE OverloadedStrings #-}

module EmailContentTests where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar (fromGregorian)
import Data.Maybe (fromJust)
import Mail.Hailgun
import Network.URI (parseURI)
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Text.HTML.TagSoup.Entity (escapeXML)

import EmailContent
import Persistence.Reminder

tests :: [Test]
tests =
  [ testProperty "Email message contains the input message" prop_emailMessageContentContainsMessage
  ]

decode = TE.decodeUtf8

prop_emailMessageContentContainsMessage :: T.Text -> Property
prop_emailMessageContentContainsMessage tIn =
  tNormalised /= "" ==> counterexample (T.unpack tNormalised ++ "\n" ++ T.unpack messageText) . property $
    tNormalisedEscaped `T.isInfixOf` messageText
  where
    tNormalised = decode . TE.encodeUtf8 $ tIn
    tNormalisedEscaped = T.pack . escapeXML . T.unpack $ tNormalised
    time = UTCTime (fromGregorian 2000 1 2) (secondsToDiffTime 30)
    uri = fromJust $ parseURI "http://test.com"
    reminder = EmailReminder 1 2 "key" "key2" "stuff" "other stuff" "userkey" "email@address" (Just tNormalised) time uri
    -- This is the actual conversion and extraction:
    messageText = decode $ htmlContent . reminderEmail $ reminder
