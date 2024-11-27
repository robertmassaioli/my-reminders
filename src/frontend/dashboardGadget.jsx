import React, { useState } from "react";
import { invoke, view } from "@forge/bridge";
import ForgeReconciler, { Text, Link, DynamicTable } from "@forge/react";
import { useEffectAsync } from "../useEffectAsync";
import moment from "moment-timezone";
import { isPresent } from "ts-is-present";
import { toDateOutput } from "./dateHelpers";
import { getSiteInfo } from "./siteInfo";

function Edit() {
  return (
    <>
      <Text>Edit Mode</Text>
    </>
  );
}

function View() {
  const [allReminders, setAllReminders] = useState(undefined);
  const [userTimezone, setUserTimezone] = useState(undefined);
  const [siteInfo, setSiteInfo] = useState(undefined);

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
      ],
    };
  });

  return (
    <>
      {!isPresent(allReminders) && <Text>Loading your reminders...</Text>}
      {isPresent(allReminders) && allReminders.length === 0 && (
        <Text>You have no reminders. View some issues and create some!</Text>
      )}
      {isPresent(allReminders) && allReminders.length > 0 && (
        <>
          <DynamicTable head={head} rows={rows} />
          <Text>You have {allReminders.length} reminders.</Text>
        </>
      )}
    </>
  );
}

function App() {
  const [context, setContext] = useState();

  useEffectAsync(async () => {
    setContext(await view.getContext());
  }, []);

  if (!context) {
    return "Loading...";
  }

  return context.extension.entryPoint === "edit" ? <Edit /> : <View />;
}

ForgeReconciler.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
);
