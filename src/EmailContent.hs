{-# LANGUAGE OverloadedStrings #-}

module EmailContent
   ( reminderEmail
   ) where

import           Data.ByteString.Lazy    (toStrict)
import           Data.Connect.Descriptor (PluginKey (..), pluginKey)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           EmailContext
import           Mail.Hailgun
import           Persistence.Reminder
import           Snap.AtlassianConnect   (connectPlugin)
import           Text.Hastache
import           Text.Hastache.Context

reminderEmail :: EmailContext -> EmailReminder -> IO MessageContent
reminderEmail emailContext reminder = do
   plainEmail <- hastacheStr defaultConfig (ecPlainEmailTemplate emailContext) (mkStrContext context)
   htmlEmail <- hastacheStr defaultConfig (ecHtmlEmailTemplate emailContext) (mkStrContext context)
   return TextAndHTML
      { textContent = toStrict . encodeUtf8 $ plainEmail
      , htmlContent = toStrict . encodeUtf8 $ htmlEmail
      }
   where
      context :: String -> MuType m
      context "baseUrl" = MuVariable . show . erTenantBaseUrl $ reminder
      context "issueKey" = MuVariable . erIssueKey $ reminder
      context "issueSummary" = MuVariable . erIssueSummary $ reminder
      context "reminderMessage" = MuVariable . erReminderMessage $ reminder
      context "originalIssueKey" = MuVariable . erOriginalIssueKey $ reminder
      context "originalIssueSummary" = MuVariable . erOriginalIssueSummary $ reminder
      context "pluginKey" = MuVariable . fromPluginKey . pluginKey . connectPlugin . ecConnectConf $ emailContext
      context "showOriginal" = MuBool originalIsDifferent
      context _ = MuNothing

      originalIsDifferent = erOriginalIssueKey reminder /= erIssueKey reminder || erOriginalIssueSummary reminder /= erIssueSummary reminder

      fromPluginKey (PluginKey textKey) = textKey
