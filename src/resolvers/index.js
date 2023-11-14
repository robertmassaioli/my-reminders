import Resolver from '@forge/resolver';
import api, { storage } from "@forge/api";
import moment from 'moment-timezone';
import { isPresent } from 'ts-is-present';
import { view } from '@forge/bridge';

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
function extractViewContext(req) {
  return {
    userAaid: req.context.accountId,
    issueId: parseInt(req.context.extension.issue.id),
    issueKey: req.context.extension.issue.key
  };
}

async function getUserTimezone(accountId) {
  const response = await api.asUser().requestJira(route`/rest/api/3/user?accountId=${accountId}`, {
    headers: {
      'Accept': 'application/json'
    }
  });

  const data =  await response.json();

  return data.timeZone;
}

async function getReminders(viewContext) {
  const result = await storage
    .entity('reminder')
    .query()
    .index("by-aaid-and-issue-id", {
      partition: [viewContext.userAaid, viewContext.issueId]
    })
    .getMany();

  return result.results;
}

resolver.define('getMyReminders', async (req) => {
  return {
    reminders: await getReminders(extractViewContext(req))
  };
});

function blankToUndefined(inputString) {
  if (!isPresent(inputString) || inputString.length === 0) {
    return undefined;
  }

  return inputString;
}

resolver.define('createReminder', async (req) => {
  const viewContext = extractViewContext(req);

  const { timestamp, issueSummary, message } = req.payload;

  const currentReminders = await getReminders(viewContext);
  if (currentReminders.length >= 10) {
    return {
      errors: `You can't create more than 10 reminders on an issue. Reminder not created.`
    };
  }

  const expiryTime = moment.unix(timestamp);

  const entityKey = `${viewContext.issueId}-${expiryTime.unix()}`;
  await storage.entity('reminder').set(entityKey, {
    issueId: viewContext.issueId,
    issueKey: viewContext.issueKey,
    originalIssueKey: viewContext.issueKey,
    issueSummary: blankToUndefined(issueSummary),
    originalIssueSummary: blankToUndefined(issueSummary),
    // TODO make sure that the message is no more than X characters long.
    message: blankToUndefined(message),
    date: expiryTime.unix(),
    day: expiryTime.format('YYYY-MM-DD'),
    userAaid: viewContext.userAaid,
    sendAttempts: 0
  });

  return {
    reminders: await getReminders(viewContext)
  }
});

resolver.define('deleteReminder', async (req) => {
  const { reminderKey } = req.payload;
  const viewContext = extractViewContext(req);

  const reminder = await storage.entity('reminder').get(reminderKey);
  if (isPresent(reminder)) {
    // Only if the current user owns the reminder should it be able to delete it
    if (reminder.userAaid === viewContext.userAaid) {
      await storage.entity('reminder').delete(reminderKey);
    } else {
      console.log(`SECURITY ALERT: User ${viewContext.userAaid} tried to delete reminder for ${reminder.userAaid} with key ${reminderKey}`);
    }
  } else {
    console.log(`Tried to delete a reminder that no longer exists: ${reminderKey} as user ${viewContext.userAaid}`)
  }

  return {
    reminders: await getReminders(viewContext)
  }
});

export const handler = resolver.getDefinitions();
