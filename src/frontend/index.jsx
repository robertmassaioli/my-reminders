import React, { useState } from 'react';
import ForgeReconciler, { Text, Heading, ButtonSet, Button, Table, Head, Cell, Row, Tooltip, Link, ModalDialog, SectionMessage, DatePicker, Select, Option, TextArea, Form } from '@forge/react';
import { invoke, requestJira, view } from '@forge/bridge';
import { useEffectAsync } from '../useEffectAsync';
import moment from 'moment-timezone';
import { isPresent } from 'ts-is-present';

function generateHoursOfDay() {
  return Array.from({ length: 24 }, (_, index) => {
    const isAfternoon = index >= 12;
    const lowerMeridien = isAfternoon ? 'pm' : 'am';
    let rawHour = index >= 12? index - 12 : index;
    const displayLowerHour = rawHour === 0 ? 12 : rawHour;
    const displayUpperHour = rawHour + 1;
    const upperIs12 = displayUpperHour === 12;
    const upperMeridien = isAfternoon ? (upperIs12 ? 'am' : 'pm') : (upperIs12 ? 'PM' : 'AM');
    return {
      index,
      display: `${displayLowerHour}${lowerMeridien}-${displayUpperHour}${upperMeridien}`
    };
  });
}

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

const App = () => {
  const [isAddReminderOpen, setAddReminderOpen] = useState(false);
  const [reminders, setReminders] = useState(undefined);
  const [userTimezone, setUserTimezone] = useState(undefined);
  const [expiredRemindersWebtrigger, setExpiredRemindersWebtrigger] = useState(undefined);

  useEffectAsync(async () => {
    setExpiredRemindersWebtrigger(await invoke('getExpirySchedulerWebTrigger'));
  }, expiredRemindersWebtrigger);

  useEffectAsync(async () => {
    const data = await invoke('getMyReminders');
    setReminders(data.reminders);
  }, reminders);

  useEffectAsync(async () => {
    const context = await view.getContext();
    setUserTimezone(context.timezone);
  }, userTimezone);

  if (isPresent(userTimezone)) {
    moment.tz.setDefault(userTimezone);
  }

  async function createReminderForTomorrow() {
    const context = await view.getContext();
    const issueSummary = await getIssueSummary(context);

    const tomorrow = moment().add(1, 'day').set('hour', 6 + Math.round(Math.random()*2));

    const response = await createReminder({
      issueSummary,
      timestamp: tomorrow.unix(),
      message: undefined
    });

    if (isPresent(response.errors)) {
      // TODO How do I flash the errors?
    } else {
      setReminders(response.reminders);
    }
  }

  async function createReminderForNextWeek() {
    const context = await view.getContext();
    const issueSummary = await getIssueSummary(context);

    const nextWeek = moment().add(7, 'day').set('hour', 6 + Math.round(Math.random()*2));

    const response = await createReminder({
      issueSummary,
      timestamp: nextWeek.unix(),
      message: undefined
    });

    if (isPresent(response.errors)) {
      // TODO How do I flash the errors?
    } else {
      setReminders(response.reminders);
    }
  }

  async function createArbitraryReminder({ expiryDate, window, message }) {
    const context = await view.getContext();
    const issueSummary = await getIssueSummary(context);

    const expiry = moment(expiryDate).hour(window.value);
    console.log(expiry.format('YYYY-MM-DD h:ma'));

    const response = await createReminder({
      issueSummary,
      timestamp: expiry.unix(),
      message
    });

    if (isPresent(response.errors)) {
      // TODO How do I flash the errors?
    } else {
      setReminders(response.reminders);
    }
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
        <Button  onClick={() => createReminderForNextWeek()}>In a Week</Button>
        <Button onClick={() => setAddReminderOpen(true)}>Select a time...</Button>
      </ButtonSet>
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
      <Text>Your timezone: <Link href="/secure/ViewPersonalSettings.jspa">{userTimezone}</Link></Text>
      {isAddReminderOpen && (
        <ModalDialog header="Add a Reminder" onClose={() => setAddReminderOpen(false)}>
          <Form
            onSubmit={(data) => {
              setAddReminderOpen(false);
              createArbitraryReminder(data);
            }}
          >
            <DatePicker
              defaultValue={moment().add(1, 'day').format('YYYY-MM-DD')}
              name="expiryDate"
              label='Expiry date'
              description='Set this date to the day that you want your reminder to be sent'
              isRequired={true}
            />
            <Select
              name="window"
              label='Hour of day'
              description='In which hour do you want your reminder to be sent?'
              >
              {generateHoursOfDay().map(hourOfDay => {
                return (
                  <Option label={hourOfDay.display} value={hourOfDay.index} defaultSelected={hourOfDay.index === 5} />
                );
              })}
            </Select>
            <TextArea
              label="Reminder message"
              name="message"
              placeholder="Optional message to go with your reminder"
            />
          </Form>
        </ModalDialog>
      )}
      {/* <Text>Reminder Data: {JSON.stringify(reminders, null, 2)}</Text> */}
      {/* <Text>Check for expired Reminders: {expiredRemindersWebtrigger}</Text> */}
      {/* <Text>Issue Summary: {issueSummary}</Text> */}
    </>
  );
};

ForgeReconciler.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
);
