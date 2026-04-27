import React from "react";
import ForgeReconciler, { Text } from "@forge/react";
import { invoke } from "@forge/bridge";
import { useRemindersData } from "./useRemindersData";
import { RemindersTable } from "./remindersTable";

const App = () => {
  const { allReminders, setAllReminders, siteInfo } = useRemindersData();

  async function deleteReminder(reminderKey) {
    const data = await invoke("deleteReminder", { reminderKey });
    setAllReminders(data.reminders);
  }

  return (
    <>
      <Text>
        All of your pending reminders can be viewed here. You can also perform
        some bulk actions on them.
      </Text>
      <RemindersTable
        reminders={allReminders}
        siteInfo={siteInfo}
        onDelete={deleteReminder}
      />
    </>
  );
};

ForgeReconciler.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
);
