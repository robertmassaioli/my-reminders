import React, { useState } from 'react';
import ForgeReconciler, { Text, Heading, ButtonSet, Button, Table, Head, Cell, Row, Tooltip, Link, ModalDialog, SectionMessage, DatePicker, Select, Option, TextArea, Form } from '@forge/react';
import { invoke, requestJira, view } from '@forge/bridge';
import { useEffectAsync } from '../useEffectAsync';
import moment from 'moment-timezone';
import { isPresent } from 'ts-is-present';
import { toDateOutput } from './dateHelpers';

async function getSiteInfo() {
  const response = await requestJira(`/rest/api/3/serverInfo`, {
    headers: {
      'Content-type': 'application/json',
      'Accept': 'application/json'
    }
  });
  return await response.json();
}

const App = () => {
  const [allReminders, setAllReminders] = useState(undefined);
  const [siteInfo, setSiteInfo] = useState(undefined);
  const [userTimezone, setUserTimezone] = useState(undefined);

  useEffectAsync(async () => {
    setSiteInfo(await getSiteInfo());
  }, siteInfo);

  useEffectAsync(async () => {
    const response = await invoke('getYourReminders');
    setAllReminders(response.reminders);
  }, allReminders);

  useEffectAsync(async () => {
    const context = await view.getContext();
    setUserTimezone(context.timezone);
  }, userTimezone);

  if (isPresent(userTimezone)) {
    moment.tz.setDefault(userTimezone);
  }

  async function deleteReminder(reminderKey) {
    const data = await invoke('deleteReminder', { reminderKey });

    console.log(JSON.stringify(data.reminders, null, 2));
    setAllReminders(data.reminders);
  }

  return (
    <>
      <Text>All of your pending reminders can be viewed here. You can also perform some bulk actions on them.</Text>
      {!isPresent(allReminders) && (
        <Text>Loading your reminders...</Text>
      )}
      {isPresent(allReminders) && allReminders.length === 0 && (
        <Text>You have no reminders. View some issues and create some!</Text>
      )}
      {isPresent(allReminders) && allReminders.length > 0 && (
        <>
          <Table>
            <Head>
              <Cell><Text>Date</Text></Cell>
              <Cell><Text>Issue</Text></Cell>
              <Cell><Text>Message</Text></Cell>
              <Cell><Text>Action</Text></Cell>
            </Head>
            {allReminders.map(reminderResult => {
              const reminder = reminderResult.value;
              const expiry = moment.unix(reminder.date);
              return (
                <Row>
                  <Cell>{toDateOutput(expiry)}</Cell>
                  <Cell><Link href={siteInfo ? `${siteInfo.displayUrl}/browse/${reminder.issueKey}` : '#'}>{reminder.issueKey}</Link></Cell>
                  <Cell>{reminder.message}</Cell>
                  <Cell><Button icon="editor-remove" onClick={() => deleteReminder(reminderResult.key)} /></Cell>
                </Row>
              );
            })}
          </Table>
        </>
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