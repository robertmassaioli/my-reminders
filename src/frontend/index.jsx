import React, { useEffect, useState } from 'react';
import ForgeReconciler, { Text, Heading, ButtonSet, Button, Table, Head, Cell, Row, Tooltip, Link, ModalDialog, SectionMessage } from '@forge/react';
import { invoke, requestJira, view } from '@forge/bridge';
import { useEffectAsync } from '../useEffectAsync';
import moment from 'moment-timezone';
import { isPresent } from 'ts-is-present';

async function createReminder({ issueSummary, timestamp, message }) {
  const response = await invoke('createReminder', {
    issueSummary,
    timestamp,
    message
  });

  return response;
}

async function getIssueSummary(context) {
  const response = await requestJira(`/rest/api/3/issue/${context.extension.issue.id}`, {
    headers: {
      'Accept': 'application/json'
    }
  });
  const issueData = await response.json();
  return issueData.fields.summary;
}

async function getUserTimezone(context) {
  const response = await requestJira(`/rest/api/3/user?accountId=${context.accountId}`, {
    headers: {
      'Accept': 'application/json'
    }
  });
  const data = await response.json();
  return data.timezone;
}

const App = () => {
  const [isAddReminderOpen, setAddReminderOpen] = useState(false);
  const [reminders, setReminders] = useState(undefined);
  const [userTimezone, setUserTimezone] = useState(undefined);

  useEffectAsync(async () => {
    const data = await invoke('getMyReminders');
    setReminders(data.reminders);
  }, reminders);

  useEffectAsync(async () => {
    const context = await view.getContext();
    const data = await getUserTimezone(context);
    setUserTimezone(data.timeZone);
  }, userTimezone);

  if (isPresent(userTimezone)) {
    moment.tz.setDefault(userTimezone);
  }

  async function createReminderForTomorrow() {
    const context = await view.getContext();
    const issueSummary = await getIssueSummary(context);

    // TODO Randomise between 6 and 8 am
    const tomorrow = moment().add(1, 'day').set('hour', 7);

    const response = await createReminder({
      issueSummary,
      timestamp: tomorrow.unix(),
      message: undefined
    });

    setReminders(response.reminders);
  }

  async function deleteReminder(reminderKey) {
    const data = await invoke('deleteReminder', { reminderKey });

    console.log(JSON.stringify(data.reminders, null, 2));
    setReminders(data.reminders);
  }

  return (
    <>
      <Heading size="medium">Add reminder</Heading>
      <ButtonSet>
        <Button onClick={() => createReminderForTomorrow()}>Tomorrow</Button>
        <Button>In a Week</Button>
        <Button onClick={() => setAddReminderOpen(true)}>Select a time...</Button>
      </ButtonSet>
      <Text>Reminder Data: {JSON.stringify(reminders, null, 2)}</Text>
      {/* <Text>Issue Summary: {issueSummary}</Text> */}
      {reminders && reminders.length > 0 && (
        <>
          <Heading size="medium">Your reminders</Heading>
          <Table>
            <Head>
              <Cell><Text>When</Text></Cell>
              <Cell><Text>Message</Text></Cell>
              <Cell><Text>Action</Text></Cell>
            </Head>
            {reminders.map(reminder => {
              const { date, message } = reminder.value;
              const expiry = moment.unix(date);
              const dateOutput = expiry.format('D MMM YYYY');
              const hourOutput = expiry.format('h');
              const meridian = expiry.format('A');
              expiry.add(1, 'hour');
              const nextHourOutput = expiry.format('h');

              const fullDateOutput = `${dateOutput} at ${hourOutput}-${nextHourOutput}${meridian}`;
              return (
                <Row>
                  <Cell>
                    <Tooltip text={fullDateOutput}><Text>{expiry.fromNow()}</Text></Tooltip>
                  </Cell>
                  <Cell>
                    <Text>{message || '<No Message>'}</Text>
                  </Cell>
                  <Cell><Button icon="editor-remove" onClick={() => deleteReminder(reminder.key)} /></Cell>
                </Row>
              );
            })}
          </Table>
        </>
      )}
      {reminders && reminders.length === 0 && (
        <SectionMessage>
          <Text>You have no reminders on this issue. Create some!</Text>
        </SectionMessage>
      )}
      {reminders === undefined && (
        <SectionMessage>
          <Text>Loading your reminders...</Text>
        </SectionMessage>
      )}
      <Text>Your timezone: <Link href="/secure/ViewPersonalSettings.jspa">{userTimezone || "Loading..."}</Link></Text>
      {isAddReminderOpen && (
        <ModalDialog header="Add a Reminder" onClose={() => setAddReminderOpen(false)}>
          <Text>Hello!</Text>
        </ModalDialog>
      )}
    </>
  );

  /*
  return (
    <>
      <Text>Hello world!</Text>
      <Text>{data ? data : 'Loading...'}</Text>
    </>
  );
  */
};

ForgeReconciler.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
);
