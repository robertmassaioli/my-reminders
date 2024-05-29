{-# LANGUAGE OverloadedStrings #-}

module EmailContent
   ( reminderEmail
   , MessageContent(..)
   ) where

import qualified Data.ByteString as B
import           Data.Connect.Descriptor (PluginKey (..), pluginKey)
import           Data.Text.Encoding (encodeUtf8)
import           EmailContext
import           Persistence.Reminder
import qualified Snap.AtlassianConnect as AC
import qualified Text.Mustache as M
import Data.Time.Clock.POSIX
import Data.Time.Clock

data MessageContent = TextAndHTML
  { textContent :: B.ByteString
  , htmlContent :: B.ByteString
  }

data Context = Context
   { cTenant :: AC.Tenant
   , cReminder :: Reminder
   , cEmailContext :: EmailContext
   }

utcTimeToUnixTimestamp :: UTCTime -> Integer
utcTimeToUnixTimestamp utcTime = round $ utcTimeToPOSIXSeconds utcTime

instance M.ToMustache Context where
   toMustache context = M.object
      [ "baseUrl" M.~> (show . AC.baseUrl . cTenant $ context)
      , "issueKey" M.~> reminderIssueKey reminder
      , "issueSummary" M.~> reminderIssueSummary reminder
      , "reminderMessage" M.~> reminderMessage reminder
      , "reminderDateUnix" M.~> (utcTimeToUnixTimestamp . reminderDate $ reminder)
      , "originalIssueKey" M.~> reminderOriginalIssueKey reminder
      , "originalIssueSummary" M.~> reminderOriginalIssueSummary reminder
      , "pluginKey" M.~> (fromPluginKey . pluginKey . AC.connectPlugin . ecConnectConf . cEmailContext $ context)
      , "showOriginal" M.~> originalIsDifferent
      ]
      where
         reminder = cReminder context
         originalIsDifferent = reminderOriginalIssueKey reminder /= reminderIssueKey reminder || reminderOriginalIssueSummary reminder /= reminderIssueSummary reminder
         fromPluginKey (PluginKey textKey) = textKey


reminderEmail :: AC.Tenant -> EmailContext -> Reminder -> MessageContent
reminderEmail tenant emailContext reminder =
   TextAndHTML
      { textContent = encodeUtf8 $ M.substitute (ecPlainEmailTemplate emailContext) context
      , htmlContent = encodeUtf8 $ M.substitute (ecHtmlEmailTemplate emailContext) context
      }
   where
      context :: Context
      context = Context
         { cTenant = tenant
         , cReminder = reminder
         , cEmailContext = emailContext
         }
