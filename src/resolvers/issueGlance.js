import Resolver from '@forge/resolver';
import api, { storage, webTrigger } from "@forge/api";
import moment from 'moment-timezone';
import { isPresent } from 'ts-is-present';
import { deleteReminder } from './reminderPersistence';

const resolver = new Resolver();

function extractViewContext(req) {
  return {
    userAaid: req.context.accountId,
    issueId: parseInt(req.context.extension.issue.id),
    issueKey: req.context.extension.issue.key
  };
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

  await deleteReminder(reminderKey, viewContext.userAaid);

  return {
    reminders: await getReminders(viewContext)
  }
});

resolver.define('getExpirySchedulerWebTrigger', async () => {
  return await webTrigger.getUrl("sendExpiredReminders");
});

export const issueGlanceHandler = resolver.getDefinitions();
