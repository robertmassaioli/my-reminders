import React, { useState } from "react";
import { view } from "@forge/bridge";
import ForgeReconciler, { Text } from "@forge/react";
import { useEffectAsync } from "../useEffectAsync";
import { useRemindersData } from "./useRemindersData";
import { RemindersTable } from "./remindersTable";

function Edit() {
  return <Text>Edit Mode</Text>;
}

function View() {
  const { allReminders, siteInfo } = useRemindersData();

  return (
    <>
      <RemindersTable reminders={allReminders} siteInfo={siteInfo} />
      {allReminders && allReminders.length > 0 && (
        <Text>You have {allReminders.length} reminders.</Text>
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
    return <Text>Loading...</Text>;
  }

  return context.extension.entryPoint === "edit" ? <Edit /> : <View />;
}

ForgeReconciler.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
);
