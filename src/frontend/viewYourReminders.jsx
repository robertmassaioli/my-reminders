import React, { useState } from "react";
import ForgeReconciler, {
  Text,
  Button,
  Link,
  DynamicTable,
} from "@forge/react";
import { invoke, view } from "@forge/bridge";
import { useEffectAsync } from "../useEffectAsync";
import moment from 'moment-timezone/builds/moment-timezone-with-data';
import { isPresent } from "ts-is-present";
import { toDateOutput } from "./dateHelpers";
import { getSiteInfo } from "./siteInfo";

const App = () => {
  const [allReminders, setAllReminders] = useState(undefined);
  const [siteInfo, setSiteInfo] = useState(undefined);
  const [userTimezone, setUserTimezone] = useState(undefined);

  useEffectAsync(async () => {
    setSiteInfo(await getSiteInfo());
  }, []);

  useEffectAsync(async () => {
    const response = await invoke("getYourReminders");
    setAllReminders(response.reminders);
  }, []);

  useEffectAsync(async () => {
    const context = await view.getContext();
    setUserTimezone(context.timezone);
  }, []);

  if (isPresent(userTimezone)) {
    moment.tz.setDefault(userTimezone);
  }

  async function deleteReminder(reminderKey) {
    const data = await invoke("deleteReminder", { reminderKey });

    setAllReminders(data.reminders);
  }
  const head = {
    cells: [
      {
        key: "date",
        content: "Date",
      },
      {
        key: "issue",
        content: "Issue",
      },
      {
        key: "message",
        content: "Message",
      },
      {
        key: "action",
        content: "Action",
      },
    ],
  };

  const rows = (allReminders || []).map((reminderResult, index) => {
    const reminder = reminderResult.value;
    const expiry = moment.unix(reminder.date);

    return {
      key: `row-${index}-${Date.now()}`,
      cells: [
        {
          key: `expiry-${Date.now()}`,
          content: toDateOutput(expiry),
        },
        {
          key: `issueKey-${Date.now()}`,
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
          key: `message-${Date.now()}`,
          content: reminder.message,
        },
        {
          key: `remove-${Date.now()}`,
          content: (
            <Button
              iconBefore="editor-remove"
              onClick={() => deleteReminder(reminderResult.key)}
            >Delete reminder</Button>
          ),
        },
      ],
    };
  });

  return (
    <>
      <Text>
        All of your pending reminders can be viewed here. You can also perform
        some bulk actions on them.
      </Text>
      {!isPresent(allReminders) && <Text>Loading your reminders...</Text>}
      {isPresent(allReminders) && allReminders.length === 0 && (
        <Text>You have no reminders. View some issues and create some!</Text>
      )}
      {isPresent(allReminders) && allReminders.length > 0 && (
        <DynamicTable isFixedSize={true} head={head} rows={rows} />
      )}
      {/* <Text>{JSON.stringify(allReminders)}</Text> */}
    </>
  );
};

ForgeReconciler.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
);
