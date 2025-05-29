./manifest.dhall /\ {
  app.connect.key = "com.atlassian.myreminders.local",
  modules =
    { webtrigger =
      [ { function = "expiryScheduler", key = "sendExpiredReminders" } ]
    },
}