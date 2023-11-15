import Resolver from '@forge/resolver';
import api, { route, storage, webTrigger } from "@forge/api";
import moment from 'moment-timezone';
import { isPresent } from 'ts-is-present';

const resolver = new Resolver();

resolver.define('getText', (req) => {
  console.log(req);

  return 'Hello, world!';
});

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

// Stole from myself here: https://bitbucket.org/atlassian-developers/docs-feedback-report/src/master/src/main.ts
export function tag(name, ...children) {
  if (children.length === 0) {
      return `<${name} />`;
  }
  return `<${name}>${children.join('')}</${name}>`;
}

export function tagWithAttributes(name, attributes, ...children) {
  if (Object.keys(attributes).length === 0) {
      return tag(name, ...children);
  }
  const prefix = `<${name} ${Object.keys(attributes).map(name => `${name}="${attributes[name]}"`).join(' ')}`;
  if (children.length === 0) {
      return `${prefix} />`;
  }

  return `${prefix}>${children.join('')}</${name}>`;
}

function generateHtmlBody(reminder) {
  const showOriginal = reminder.issueKey !== reminder.originalIssueKey || reminder.issueSummary !== reminder.originalIssueSummary;
  return tag('div',
    ... (!reminder.message ? [] : [
      tagWithAttributes('p', { style: 'margin: 10px 0 0 0; margin-top: 0' },
        reminder.message
      ),
      tag('br')
    ]),
    ... (!showOriginal ? [] : [
      tagWithAttributes('p', { style: '' },
        `Your original reminder was called: ${reminder.originalIssueKey} - ${reminder.originalIssueSummary}`
      ),
      tag('br')
    ])
  );
}

function generateTextBody(reminder) {
  const showOriginal = reminder.issueKey !== reminder.originalIssueKey || reminder.issueSummary !== reminder.originalIssueSummary;
  return [
    `Reminder for ${reminder.issueKey}: ${reminder.issueSummary}`,
    '',
    ...(!reminder.message ? [] : [
      'Your message:',
      '',
      reminder.message,
      ''
    ]),
    ...(!showOriginal ? [] : [
      `Your original reminder was called: ${reminder.originalIssueKey} - ${reminder.originalIssueSummary}`,
      ''
    ]),
    'This was created using the My Reminders add-on.'
  ].join('\n');
}

// TODO: Move this into a different resolver for observability
resolver.define('sendExpiredReminder', async ({ payload, context }) => {
  const reminderKey = payload.key;
  const reminder = payload.value;
	// process the event
  console.log(`Sending reminder: ${reminderKey}`);

  try {
    // Attempt to send the notification
    console.log(`/rets/api/3/issue/${reminder.issueId}/notify`);
    const subject = `Reminder: [${reminder.issueKey}] ${reminder.issueSummary}`;
    const textBody = generateTextBody(reminder);
    const htmlBody = generateHtmlBody(reminder);
    const to = {
      users: [{ accountId: reminder.userAaid }]
    };
    const jsonBody = {
      subject,
      textBody,
      htmlBody,
      to
    };
    // console.log(JSON.stringify(jsonBody));
    await api.asApp().requestJira(route`/rest/api/3/issue/${reminder.issueId}/notify`, {
      method: 'POST',
      headers: {
        'Accept': 'application/json',
        'x-atlassian-force-account-id': 'true'
      },
      body: JSON.stringify(jsonBody)
    });
    console.log(`Sent reminder: ${reminderKey}`);

    // Delete the reminder from storage
    await storage.entity('reminder').delete(reminderKey);
    console.log(`Cleared reminder: ${reminderKey}`);
  } catch (e) {
    // TODO throw an error to trigger the retry logic
    console.log(`Error sending: ${reminderKey}: ${e}`);
  }
});

resolver.define('getExpirySchedulerWebTrigger', async () => {
  return await webTrigger.getUrl("sendExpiredReminders");
});

export const handler = resolver.getDefinitions();
