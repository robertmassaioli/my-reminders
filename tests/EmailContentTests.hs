{-# LANGUAGE OverloadedStrings #-}
module EmailContentTests where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar (fromGregorian)
import Data.Maybe (fromJust)
import Network.URI (parseURI, URI)
import EmailContext
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Instances ()
import Text.HTML.TagSoup.Entity (escapeXML)
import qualified Snap.AtlassianConnect as AC
import qualified Text.Mustache as M
import qualified Data.Connect.Descriptor as D

import EmailContent
import Persistence.Reminder

tests :: [Test]
tests =
  [ testProperty "Email message contains the input message" prop_emailMessageContentContainsMessage
  ]

decode = TE.decodeUtf8

fakeTenant :: AC.Tenant
fakeTenant = AC.Tenant
  { AC.tenantId = 2143
  , AC.key = "unique-identifier"
  , AC.publicKey = "23823g4982384927"
  , AC.sharedSecret = "1234123412341234"
  , AC.baseUrl = AC.CURI . toURI $ "https://your-domain.atlassian.net"
  , AC.productType = "jira"
  }

rightOrError :: Show a => Either a b -> b
rightOrError (Left l) = error $ "It was a Left value" ++ (show l)
rightOrError (Right x) = x

toURI = fromJust . parseURI

prop_emailMessageContentContainsMessage :: T.Text -> Property
prop_emailMessageContentContainsMessage tIn = monadicIO $ do
  messageText <- run getMessageText
  pre $ tNormalised /= ""
  pre . and $ fmap (\x -> not $ T.pack x `T.isInfixOf` tNormalised) ["'", "\\"]
  assert $ tNormalisedEscaped `T.isInfixOf` messageText
  where
    tNormalised = decode . TE.encodeUtf8 $ tIn
    tNormalisedEscaped = T.pack . escapeXML . T.unpack $ tNormalised
    time = UTCTime (fromGregorian 2000 1 2) (secondsToDiffTime 30)
    reminder = Reminder 1 2 12345 "originalKey" "newKey" "stuff" "other stuff" "useraaid" (Just tNormalised) time 0
    rawTemplate = T.pack "{{reminderMessage}}"
    plainTemplate = rightOrError $ M.compileTemplate "plain-template" rawTemplate
    htmlTemplate = rightOrError $ M.compileTemplate "html-template" rawTemplate
    emailContext = EmailContext
       { ecConnectConf = connect
       , ecPlainEmailTemplate = plainTemplate
       , ecHtmlEmailTemplate = htmlTemplate
       }
    cBaseUrl = toURI "https://my-app.my-domain.app/path"
    connect = AC.Connect
      { AC.connectAES = undefined
      , AC.connectPageTokenTimeout = undefined
      , AC.connectPlugin = exampleDescriptor cBaseUrl
      , AC.connectBaseUrl = cBaseUrl
      , AC.connectHostWhitelist = []
      }
    -- This is the actual conversion and extraction:
    getMessageText :: IO T.Text
    getMessageText = do
      let messageContent = reminderEmail fakeTenant emailContext reminder
      return . decode . htmlContent $ messageContent

exampleDescriptor :: URI -> D.Plugin
exampleDescriptor baseURL = (D.pluginDescriptor (D.PluginKey "my-example-connect") baseURL (D.Authentication D.Jwt))