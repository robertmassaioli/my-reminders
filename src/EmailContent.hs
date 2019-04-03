{-# LANGUAGE OverloadedStrings #-}

module EmailContent
   ( reminderEmail
   , MessageContent(..)
   ) where

import qualified Data.ByteString as B
import           Data.ByteString.Lazy    (toStrict)
import           Data.Connect.Descriptor (PluginKey (..), pluginKey)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           EmailContext
import           Persistence.Reminder
import           Snap.AtlassianConnect   (connectPlugin, Tenant(..))
import           Text.Hastache
import           Text.Hastache.Context

data MessageContent = TextAndHTML
  { textContent :: B.ByteString
  , htmlContent :: B.ByteString
  }

reminderEmail :: Tenant -> EmailContext -> Reminder -> IO MessageContent
reminderEmail tenant emailContext reminder = do
   plainEmail <- hastacheStr defaultConfig (ecPlainEmailTemplate emailContext) (mkStrContext context)
   htmlEmail <- hastacheStr defaultConfig (ecHtmlEmailTemplate emailContext) (mkStrContext context)
   return TextAndHTML
      { textContent = toStrict . encodeUtf8 $ plainEmail
      , htmlContent = toStrict . encodeUtf8 $ htmlEmail
      }
   where
      context :: String -> MuType m
      context "baseUrl" = MuVariable . show . baseUrl $ tenant
      context "issueKey" = MuVariable . reminderIssueKey $ reminder
      context "issueSummary" = MuVariable . reminderIssueSummary $ reminder
      context "reminderMessage" = MuVariable . reminderMessage $ reminder
      context "originalIssueKey" = MuVariable . reminderOriginalIssueKey $ reminder
      context "originalIssueSummary" = MuVariable . reminderOriginalIssueSummary $ reminder
      context "pluginKey" = MuVariable . fromPluginKey . pluginKey . connectPlugin . ecConnectConf $ emailContext
      context "showOriginal" = MuBool originalIsDifferent
      context _ = MuNothing

      originalIsDifferent = reminderOriginalIssueKey reminder /= reminderIssueKey reminder || reminderOriginalIssueSummary reminder /= reminderIssueSummary reminder

      fromPluginKey (PluginKey textKey) = textKey
