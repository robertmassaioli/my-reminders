import Resolver from '@forge/resolver';
import api, { storage } from "@forge/api";

const resolver = new Resolver();

resolver.define('getText', (req) => {
  console.log(req);

  return 'Hello, world!';
});

/*
{
  "payload": {},
  "context": {
    "accountId": "557057:9eb9edaf-b755-4dd1-ab28-5c3f68948790",
    "localId": "ari:cloud:ecosystem::extension/947ac65b-ca66-412f-8e52-11e797583c52/abae6ab2-7dfe-414f-a4c3-a99c8ab53da8/static/view-issue-glance-reminders-v2",
    "cloudId": "96fb4616-6245-4043-ba30-4adbb684d94f",
    "moduleKey": "view-issue-glance-reminders-v2",
    "extension": {
      "issue": {
        "key": "HT-12",
        "id": "11063",
        "type": "ShipIt Project",
        "typeId": "10100"
      },
      "project": {
        "id": "10101",
        "key": "HT",
        "type": "software"
      },
      "type": "jira:issueContext"
    },
    "accountType": "licensed",
    "installContext": "ari:cloud:jira::site/96fb4616-6245-4043-ba30-4adbb684d94f"
  }
}
*/
resolver.define('getMyReminders', async (req) => {
  const newResult = await storage
    .entity('reminder')
    .query()
    .index('by-key')
    .getMany();

  console.log(JSON.stringify(newResult, null, 2))

  const viewContext = {
    userAaid: req.context.accountId,
    issueId: req.context.extension.issue.id
  };
  console.log(JSON.stringify(viewContext));
  const result = await storage
    .entity('reminder')
    .query()
    .index("by-aaid-and-issue-id-v2", {
      partition: [viewContext.userAaid, viewContext.issueId]
    })
    .getMany();

  return result.results;
});

export const handler = resolver.getDefinitions();
