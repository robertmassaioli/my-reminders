{ app =
  { connect =
    { authentication = "jwt"
    , remote = "connect"
    }
  , features.autoUserConsent = True
  , id = "ari:cloud:ecosystem::app/947ac65b-ca66-412f-8e52-11e797583c52"
  -- Experienced an error trying to make REST calls when this was enabled: https://atlassian.slack.com/archives/C02DJFQC8G7/p1701819244863659
  , runtime.name = "nodejs18.x"
  , storage.entities
    =
    [ { attributes =
        { date.type = "integer"
        , day.type = "string"
        , issueId.type = "integer"
        , issueKey.type = "string"
        , issueSummary.type = "string"
        , message.type = "string"
        , originalIssueKey.type = "string"
        , originalIssueSummary.type = "string"
        , sendAttempts.type = "integer"
        , userAaid.type = "string"
        }
      , indexes =
        [ { name = "expired-reminders"
          , partition = [ "day" ]
          , range = [ "date" ]
          }
        , { name = "by-aaid", partition = [ "userAaid" ], range = [ "date" ] }
        , { name = "by-issue-id"
          , partition = [ "issueId" ]
          , range = [ "date" ]
          }
        , { name = "by-aaid-and-issue-id"
          , partition = [ "userAaid", "issueId" ]
          , range = [ "date" ]
          }
        , { name = "by-aaid-and-issue-id-v2"
          , partition = [ "userAaid", "issueId" ]
          , range = [ "date" ]
          }
        ]
      , name = "reminder"
      }
    ]
  }
, connectModules =
  { `jira:generalPages` =
    [ { conditions = None (List { condition : Text, invert : Bool })
      , key = "view-my-reminders"
      , location = "system.user.options/personal"
      , name.value = "My Reminders (legacy)"
      , url = "/panel/jira/reminders/view?forge=true"
      }
    , { conditions = Some
        [ { condition = "user_is_logged_in", invert = False } ]
      , key = "create-reminder-dialog"
      , location = "completely-invalid-location"
      , name.value = "Create reminder (legacy)"
      , url =
          "/panel/v2/jira/reminder/create?issue_key={issue.key}&issue_id={issue.id}"
      }
    ]
  , `jira:jiraIssueGlances` =
    [ { content = { label.value = "My Reminders", type = "label" }
      , icon = { height = 24, url = "/static/frontend/logo.svg", width = 24 }
      , key = "view-issue-glance-reminders"
      , name.value = "My Reminders (legacy)"
      , target =
        { type = "web_panel"
        , url =
            "/panel/jira/reminder/simple?issue_key={issue.key}&issue_id={issue.id}&forge=true"
        }
      }
    ]
  , `jira:lifecycle` =
    [ { installed = "/installed"
      , key = "lifecycle-events"
      , uninstalled = "/uninstalled"
      }
    ]
  , `jira:webPanels` =
    [ { conditions = [ { condition = "user_is_logged_in", invert = False } ]
      , key = "view-issue-reminders"
      , location = "atl.jira.view.issue.right.context"
      , name.value = "My reminders (legacy)"
      , tooltip.value = "Your reminders for this issue."
      , url =
          "/panel/jira/reminder/simple?issue_key={issue.key}&issue_id={issue.id}&forge=true"
      }
    ]
  , `jira:webhooks` =
    [ { event = "jira:issue_updated"
      , key = "webhook-1"
      , url = "/rest/webhook/issue/update"
      }
    , { event = "jira:issue_deleted"
      , key = "webhook-2"
      , url = "/rest/webhook/issue/delete"
      }
    ]
  }
, modules =
  { consumer =
    [ { key = "expiredReminderSender"
      , queue = "expiredReminders"
      , resolver =
        { function = "sendRemindersResolver", method = "sendExpiredReminder" }
      }
    ]
  , function =
    [ { handler = "index.scheduleExpiryJobs", key = "expiryScheduler" }
    , { handler = "index.issueGlanceHandler", key = "issueGlanceResolver" }
    , { handler = "index.sendRemindersHandler", key = "sendRemindersResolver" }
    , { handler = "index.yourRemindersHandler", key = "yourRemindersResolver" }
    ]
  , `jira:globalPage` =
    [ { key = "view-my-reminders-v2"
      , render = "native"
      , resolver.function = "yourRemindersResolver"
      , resource = "yourRemindersMain"
      , title = "Your reminders"
      }
    ]
  , `jira:issueContext` =
    [ { key = "view-issue-glance-reminders-v2"
      , label = "Create issue reminders"
      , render = "native"
      , resolver.function = "issueGlanceResolver"
      , resource = "issueGlanceMain"
      , title = "My Reminders"
      }
    ]
  , scheduledTrigger =
    [ { function = "expiryScheduler"
      , interval = "hour"
      , key = "expirySchedulerJob"
      }
    ]
  , webtrigger =
    [ { function = "expiryScheduler", key = "sendExpiredReminders" } ]
  }
, permissions.scopes
  =
  [ "storage:app"
  , "read:connect-jira"
  , "write:connect-jira"
  , "read:jira-work"
  , "write:jira-work"
  ]
, resources =
  [ { key = "issueGlanceMain", path = "src/frontend/issueGlance.jsx" }
  , { key = "yourRemindersMain", path = "src/frontend/viewYourReminders.jsx" }
  ]
}
