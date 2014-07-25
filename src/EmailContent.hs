{-# LANGUAGE OverloadedStrings #-}

module EmailContent
   ( reminderEmail
   ) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import           Text.Pandoc
import qualified Text.Pandoc.Writers.Markdown as PM
import qualified Text.Pandoc.Writers.HTML as PH
import           Persistence.Ping
import           Mail.Hailgun
import           Network.URI

reminderEmail :: EmailReminder -> MessageContent
reminderEmail reminder = TextAndHTML
   { textContent = BC.pack $ PM.writePlain emailWriterDefaults reminderDoc
   , htmlContent = BC.pack $ PH.writeHtmlString emailWriterDefaults reminderDoc
   }
   where
      reminderDoc = genericReminderEmail reminder

genericReminderEmail :: EmailReminder -> Pandoc
genericReminderEmail reminder = Pandoc nullMeta $
   [ Header 2 nullAttr [ Str $ "Issue Reminder: " ++ erIssueKey reminder ]
   , Para 
      [ Str $ "Hi " ++ (BC.unpack . erUserEmail $ reminder) ++ ","
      , LineBreak
      --, Str $ "On " ++ erPingDate reminder ++ " you set a reminder for '" -- TODO pretty print the date following the ADG, need to record the users timezone too for this, maybe don't say this?
      , Str $ "You set a reminder for '"
      , Link [] (show issueURI, erIssueSubject reminder)  -- TODO need to make sure we have the tenant avaliable so that we can tell which URL's to provide in the emails.
      , Str $ "'."
      ]
   ] ++ (message . erPingMessage $ reminder)
   where
      message :: Maybe T.Text -> [Block]
      message Nothing = []
      message (Just content) = 
         [ Para [ Str $ "You left yourself the following message:" ]
         , Para [ Str . T.unpack $ content ]
         ]

      issueURI = (erTenantBaseUrl reminder) 
         { uriPath = "/browse/" ++ erIssueKey reminder
         }

emailWriterDefaults :: WriterOptions
emailWriterDefaults = def 
   { writerStandalone = True
   }
