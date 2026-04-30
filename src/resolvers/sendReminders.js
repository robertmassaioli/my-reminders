import Resolver from '@forge/resolver';
import kvs from "@forge/kvs";
import { ForgeFunctionAdapter, asApp, ForgeApiError, NotFoundError } from '@forge-clients/core';
import { notify } from '@forge-clients/jira/v3';

const adapter = new ForgeFunctionAdapter({ product: 'jira' });

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
    const subject = `Reminder: [${reminder.issueKey}] ${reminder.issueSummary}`;
    const textBody = generateTextBody(reminder);
    const htmlBody = generateHtmlBody(reminder);

    await notify(asApp(adapter), {
      path: { issueIdOrKey: String(reminder.issueId) },
      body: {
        subject,
        textBody,
        htmlBody,
        to: {
          users: [{ accountId: reminder.userAaid }]
        }
      }
    });

    console.info(`Sent reminder: ${reminderKey}`);

    // Delete the reminder from KVS
    await kvs.entity('reminder').delete(reminderKey);
    console.info(`Cleared reminder: ${reminderKey}`);
  } catch (e) {
    if (e instanceof NotFoundError) {
      // Issue was deleted — no point retrying, remove the reminder
      console.warn(`Issue ${reminder.issueId} no longer exists; removing stale reminder ${reminderKey}`);
      await kvs.entity('reminder').delete(reminderKey);
    } else if (e instanceof ForgeApiError) {
      console.error(`API error sending reminder ${reminderKey}: ${e.statusCode} - ${e.message}`);
      // TODO: throw to trigger queue retry logic
    } else {
      console.error(`Error sending: ${reminderKey}: ${e}`);
      // TODO: throw to trigger queue retry logic
    }
  }
});

export const sendRemindersHandler = resolver.getDefinitions();