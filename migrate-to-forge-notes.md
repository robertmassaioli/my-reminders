 ## Migrate to Forge

 These are the notes that I am using to walk through my analysis of wether or not I can move to Forge for My Reminders.

 Status quo:
  - Talk securely with the connect remote. Does this require CustomUI and Forge Remote?

 Idea: I can leave the connect backend running to accept old installs and send old reminders.
  - I could put a date in place wherby I won't accept new reminders after that date unless they upgrade to the latest version, the Forge version.
 
 In order to make the viewIssueGlance work we need to:
  - Can we make these reminders move to Forge Storage instead?
    - Forge storage can support 240kb per key. No limits on the number of keys. I could work with this perhaps...
    - Max key length is 500 characters
    - Don't need the purged_tenant, tenant or installed_migrations tables
  - Don't need https://developer.atlassian.com/platform/forge/manifest-reference/modules/scheduled-trigger/
  - How do we send all of the reminders within the execution time limit? Need to use forge events?

Persistence:

Current reminder table:

```
 id                   | integer                  |           | not null | nextval('reminder_id_seq'::regclass)
 tenantid             | integer                  |           | not null | 
 issueid              | integer                  |           | not null | 
 originalissuekey     | character varying(255)   |           | not null | 
 originalissuesummary | text                     |           | not null | 
 issuekey             | character varying(255)   |           | not null | 
 issuesummary         | text                     |           | not null | 
 message              | text                     |           |          | 
 date                 | timestamp with time zone |           |          | 
 useraaid             | character varying(300)   |           | not null | 
 sendattempts         | integer                  |           | not null | 0
```

Some thoughts:
 - Don't need the id in non-relational storage
 - Don't need the tenant id because storage is per-tenant for me
 - Need the issueId (integers should take 4 bytes max)
 - Original issue key (I've put 255 bytes in for this, and, by default, this will not be more than 10 but people can increase this to 255 apparently https://support.atlassian.com/jira-cloud-administration/docs/configure-jira-application-options/ )
   - Good estimate for max key length would be 10-20 bytes.
 - Original issue summary is unbounded
   - Jira issue summary must be less than 255 characters: https://community.atlassian.com/t5/Jira-questions/Summary-must-be-less-than-255-characters/qaq-p/989632#:~:text=Summary%20must%20be%20less%20than%20255%20characters.,short%20representation%20of%20your%20issue.
 - issue key: same as the original issue key
 - issue summary: same as the original issue summary
 - message: I should put a limit on this to maybe 255 bytes and it's optional
 - date: should be a unix time
 - useraaid: This should be incorporated into the key I think.
   - maximum length is 128 characters

Putting this together:

 4 + 20*2 + 255*2 + 300 + 4 + 128 => 986 bytes / reminder (with max values)

With shorter values:

 4 + 20*2 + 100*2 + 0 + 4 + 128 => 376

With 240kb per key, that would be: 

 * 240000 / 986 => 243 reminders per issue (worst case)
 * 240000 / 376 => 638 reminders per issue (best case)

It means that we should track how much data we have stored against each key.

However, if we use the new entity storage these limits should not apply. I should go and look at those docs and talk to that team.




TODO

await storage
  .entity("reminder")
  .query()
  .index("expired-reminders", {
    partition: [<dayOfYear in 2014-08-12 format>]
  })
  .where(WhereConditions.isLessThan(<currentUnixTimestamp>))
  .getMany

await storage
  .entity("reminder")
  .query()
  .index("by-aaid", {
    partition: [<Users aaid>]
  })
  .getMany

await storage
  .entity("reminder")
  .query()
  .index("by-aaid-and-issue-id", {
    partition: [<Users aaid>, <issueId>]
  })
  .getMany
