import Resolver from '@forge/resolver';
import api, { route, storage } from "@forge/api";

const resolver = new Resolver();

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
  console.info(`Sending reminder: ${reminderKey}`);

  try {
    // Attempt to send the notification
    // console.log(`/rest/api/3/issue/${reminder.issueId}/notify`);
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
    const notifyResponse = await api.asApp().requestJira(route`/rest/api/3/issue/${reminder.issueId}/notify`, {
      method: 'POST',
      headers: {
        'Content-type': 'application/json',
        'Accept': 'application/json',
        'x-atlassian-force-account-id': 'true'
      },
      body: JSON.stringify(jsonBody)
    });
    if (!notifyResponse.ok) {
      throw new Error(`Did not send reminder for user ${reminder.userAaid} for issueId ${reminder.issueId}: ${notifyResponse.status} - ${await notifyResponse.text()}`);
    }

    console.info(`Sent reminder: ${reminderKey}`);

    // Delete the reminder from storage
    await storage.entity('reminder').delete(reminderKey);
    console.info(`Cleared reminder: ${reminderKey}`);
  } catch (e) {
    // TODO throw an error to trigger the retry logic
    console.error(`Error sending: ${reminderKey}: ${e}`);
  }
});

export const sendRemindersHandler = resolver.getDefinitions();