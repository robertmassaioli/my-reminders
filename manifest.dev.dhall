./manifest.dhall /\ {
  app.connect.key = "com.atlassian.myreminders.dev",
  remotes = [ { baseUrl = "https://my-reminders.dev.services.atlassian.com", key = "connect" } ]
}