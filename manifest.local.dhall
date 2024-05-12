./manifest.dhall /\ {
  app.connect.key = "com.atlassian.myreminders.local",
  remotes = [ { baseUrl = "https://my-reminders-public.public.atlastunnel.com", key = "connect" } ]
}