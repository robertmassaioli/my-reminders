./manifest.dhall /\ {
  app.connect.key = "com.not-atlassian.myreminders.local",
  remotes = [ { baseUrl = "https://rmassaioli.public.atlastunnel.com", key = "connect" } ]
}