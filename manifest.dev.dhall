./manifest.dhall /\ {
  app.connect.key = "com.atlassian.myreminders.dev",
  modules =
    { webtrigger =
      [ { function = "expiryScheduler", key = "sendExpiredReminders" } ]
    },
}