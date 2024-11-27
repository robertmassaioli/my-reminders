import React, { useState } from 'react';
import { invoke, view } from '@forge/bridge';
import ForgeReconciler, { Text, Table, Head, Cell, Row, Link } from '@forge/react';
import { useEffectAsync } from '../useEffectAsync';
import moment from 'moment-timezone';
import { isPresent } from 'ts-is-present';
import { toDateOutput } from './dateHelpers';
import { getSiteInfo } from './siteInfo';

function Edit() {
  return (
    <><Text>Edit Mode</Text></>
  )
}

function View() {
  const [allReminders, setAllReminders] = useState(undefined);
  const [userTimezone, setUserTimezone] = useState(undefined);
  const [siteInfo, setSiteInfo] = useState(undefined);

  useEffectAsync(async () => {
    setSiteInfo(await getSiteInfo());
  }, []);

  useEffectAsync(async () => {
    const response = await invoke('getYourReminders');
    setAllReminders(response.reminders);
  }, []);

  useEffectAsync(async () => {
    const context = await view.getContext();
    setUserTimezone(context.timezone);
  }, []);

  if (isPresent(userTimezone)) {
    moment.tz.setDefault(userTimezone);
  }

  return (
    <>
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
            </Head>
            {allReminders.map(reminderResult => {
              const reminder = reminderResult.value;
              const expiry = moment.unix(reminder.date);
              return (
                <Row>
                  <Cell>{toDateOutput(expiry)}</Cell>
                  <Cell><Link href={siteInfo ? `${siteInfo.displayUrl}/browse/${reminder.issueKey}` : '#'}>{reminder.issueKey}</Link></Cell>
                  <Cell>{reminder.message}</Cell>
                </Row>
              );
            })}
          </Table>
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
    return 'Loading...';
  }

  return context.extension.entryPoint === 'edit' ? <Edit/> : <View/>;
}

ForgeReconciler.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
);