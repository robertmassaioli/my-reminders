./manifest.dhall /\ {
  app.connect.key = "com.atlassian.myreminders",
  remotes = [ { baseUrl = "https://my-reminders.services.atlassian.com", key = "connect" } ]
}