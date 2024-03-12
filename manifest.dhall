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
    ]
  , `jira:lifecycle` =
    [ { installed = "/installed"
      , key = "lifecycle-events"
      , uninstalled = "/uninstalled"
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
    , { handler = "index.dashboardGadgetHandler", key = "dashboardGadgetResolver" }
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
  , `jira:dashboardGadget` =
    [ { key = "dashboard-view-reminders"
      , title = "Your reminders"
      , description = "See your reminders across a number of issues. See your to-do list."
      -- to-do: thumbnail
      , render = "native"
      , resource = "dashboardGadget"
      , resolver.function = "dashboardGadgetResolver"
      --, edit.resource = "dashboardGadget"
      --, edit.render = "native"
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
  , { key = "dashboardGadget", path = "src/frontend/dashboardGadget.jsx" }
  ]
}
