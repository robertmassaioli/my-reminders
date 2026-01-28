{ app =
  { id = "ari:cloud:ecosystem::app/947ac65b-ca66-412f-8e52-11e797583c52"
  -- Experienced an error trying to make REST calls when this was enabled: https://atlassian.slack.com/archives/C02DJFQC8G7/p1701819244863659
  , runtime.name = "nodejs20.x"
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
      , thumbnail = "https://marketplace.atlassian.com/product-listing/files/663e3103-e39b-4bca-82ae-23dc94c65b84?width=128&height=128"
      , render = "native"
      , resource = "dashboardGadget"
      , resolver.function = "dashboardGadgetResolver"
      --, edit.resource = "dashboardGadget"
      --, edit.render = "native"
      }
    ]
  , scheduledTrigger =
    [ { function = "expiryScheduler"
      , interval = "fiveMinute"
      , key = "expirySchedulerJob"
      }
    ]
  }
, environments.development.variables.webtriggerEnabled = "true"
, permissions.scopes
  =
  [ "storage:app"
  , "read:jira-work"
  , "send:notification:jira"
  ]
, resources =
  [ { key = "issueGlanceMain", path = "src/frontend/issueGlance.jsx" }
  , { key = "yourRemindersMain", path = "src/frontend/viewYourReminders.jsx" }
  , { key = "dashboardGadget", path = "src/frontend/dashboardGadget.jsx" }
  ]
}
