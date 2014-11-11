{-# LANGUAGE OverloadedStrings #-}

module EmailContent
   ( reminderEmail
   ) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import           Text.Pandoc
import           Persistence.Reminder
import           Mail.Hailgun
import           Network.URI

reminderEmail :: EmailReminder -> MessageContent
reminderEmail reminder = TextAndHTML
   { textContent = BC.pack $ writeMarkdown emailWriterDefaults reminderDoc
   , htmlContent = BC.pack $ writeHtmlString emailWriterDefaults reminderDoc
   }
   where
      reminderDoc = genericReminderEmail reminder

-- Note: The following email was initially generated from: pandoc-email-templates/example-email.markdown
-- And then it was modified to contain dynamic content.
-- You can generate this outline using: pandoc -t native pandoc-email-templates/example-email.markdown
genericReminderEmail :: EmailReminder -> Pandoc
genericReminderEmail reminder = Pandoc nullMeta $
   [ Header 2 ("issue-reminder--", [], []) [Str "Issue", Space, Str "Reminder", Space, Str "-", Space, Str . erIssueKey $ reminder]
   , Para [Str "Hi", Space, Str . erUserKey $ reminder, Str ","]
   , Para $ [Str "You", Space, Str "set", Space, Str "a", Space, Str "reminder", Space, Str "for:", Space, Str "'", Link [Str . erIssueSummary $ reminder] (show issueURI, ""), Str "'"] ++ originalDetails
   ] 
   ++ (message . erReminderMessage $ reminder)
   ++ [ Para [Str "Follow",Space,Str "the",Space,Str "link",Space,Str "to",Space,Str "see",Space,Str "more",Space,Str "about",Space,Str "the",Space,Str "issue."]
   , Para [Str "Cheers,",LineBreak,Str "Your",Space,Str "friendly",Space,Str "My Reminders",Space,Str "plugin."]
   ]
   where
      message :: Maybe T.Text -> [Block]
      message Nothing = []
      message (Just content) = 
         [ Para [Str "With",Space,Str "the",Space,Str "following",Space,Str "message:"]
         , BlockQuote [Para [Str . T.unpack $ content]]
         ]

      tenantURI = erTenantBaseUrl reminder
      issuePath = uriPath tenantURI ++ "/browse/" ++ erIssueKey reminder
      issueURI = tenantURI { uriPath = issuePath }

      originalDetails :: [Inline]
      originalDetails = if erOriginalIssueKey reminder /= erIssueKey reminder || erOriginalIssueSummary reminder /= erIssueSummary reminder
         then [Str ".",Space,Str "(Originally:",Space,Str . erOriginalIssueKey $ reminder ,Space,Str "-",Space,Str . erOriginalIssueSummary $ reminder, Str ")"]
         else []

emailWriterDefaults :: WriterOptions
emailWriterDefaults = def 
   --{ writerStandalone = True
   --}
