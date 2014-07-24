module EmailContent
   ( reminderEmail
   ) where

import Text.Pandoc

reminderEmail :: MessageContent

genericReminderEmail :: Ping -> Pandoc
genericReminderEmail ping = Pandoc nullMeta
   [ Header 2 nullAttr (Str "Issue Reminder: " ++ pingIssueKey ping)
   , Para 
      [ Str $ "Hi " ++ pingUserEmail ping ++ ","
      , LineBreak
      , Str $ "On " ++ pingDate ping ++ " you set a reminder for "
      , Link [] (undefined ++ "/browse/" ++ pingIssueKey ping, pingIssueSubject ping)  -- TODO need to make sure we have the tenant avaliable so that we can tell which URL's to provide in the emails.
      ]
   ]
