import React from "react";
import { Text, Button, Link, DynamicTable } from "@forge/react";
import moment from 'moment-timezone/builds/moment-timezone-with-data';
import { isPresent } from "ts-is-present";
import { toDateOutput } from "./dateHelpers";

/**
 * Shared reminder table component used by both viewYourReminders and
 * dashboardGadget. Renders a DynamicTable of reminders with date, issue
 * link, and message columns. Optionally renders a delete action column
 * when an onDelete callback is provided.
 *
 * @param {object}   props
 * @param {Array}    props.reminders    - Array of reminder result objects from KVS
 * @param {object}   props.siteInfo     - Jira site info (used for issue URLs)
 * @param {Function} [props.onDelete]   - Optional callback(reminderKey) for delete action
 */
export function RemindersTable({ reminders, siteInfo, onDelete }) {
  const hasDeleteAction = isPresent(onDelete);

  const head = {
    cells: [
      { key: "date", content: "Date" },
      { key: "issue", content: "Issue" },
      { key: "message", content: "Message" },
      ...(hasDeleteAction ? [{ key: "action", content: "Action" }] : []),
    ],
  };

  const rows = (reminders || []).map((reminderResult, index) => {
    const reminder = reminderResult.value;
    const expiry = moment.unix(reminder.date);

    return {
      key: `row-${index}`,
      cells: [
        {
          key: `expiry-${index}`,
          content: toDateOutput(expiry),
        },
        {
          key: `issueKey-${index}`,
          content: (
            <Link
              href={
                siteInfo
                  ? `${siteInfo.displayUrl}/browse/${reminder.issueKey}`
                  : "#"
              }
            >
              {reminder.issueKey}
            </Link>
          ),
        },
        {
          key: `message-${index}`,
          content: reminder.message,
        },
        ...(hasDeleteAction
          ? [
              {
                key: `remove-${index}`,
                content: (
                  <Button
                    iconBefore="editor-remove"
                    onClick={() => onDelete(reminderResult.key)}
                  >
                    Delete reminder
                  </Button>
                ),
              },
            ]
          : []),
      ],
    };
  });

  if (!isPresent(reminders)) {
    return <Text>Loading your reminders...</Text>;
  }

  if (reminders.length === 0) {
    return <Text>You have no reminders. View some issues and create some!</Text>;
  }

  return <DynamicTable isFixedSize={true} head={head} rows={rows} />;
}
