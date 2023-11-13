import React, { useEffect, useState } from 'react';
import ForgeReconciler, { Text, Heading, ButtonSet, Button, Table, Head, Cell, Row, Tooltip, Link, ModalDialog } from '@forge/react';
import { invoke } from '@forge/bridge';

const App = () => {
  const [isAddReminderOpen, setAddReminderOpen] = useState(false);
  const [reminders, setReminders] = useState(undefined);
  //const [data, setData] = useState(null);

  useEffect(() => {
    if (reminders === undefined) {
      invoke('getMyReminders').then(setReminders);
    }
  });
  // useEffect(() => {
  //   invoke('getText', { example: 'my-invoke-variable' }).then(setData);
  // }, []);

  return (
    <>
      <Heading size="medium">Add reminder</Heading>
      <ButtonSet>
        <Button text="Tomorrow" onClick={async () => setReminders(await createReminderForTomorrow(viewContext))} />
        <Button text="In a Week" />
        <Button text="Select a time..." onClick={() => setAddReminderOpen(true)}/>
      </ButtonSet>
      <Heading size="medium">Your reminders</Heading>
      <Text>Reminder Data: {JSON.stringify(reminders, null, 2)}</Text>
      {reminders && (
        <Table>
        <Head>
          <Cell><Text>When</Text></Cell>
          <Cell><Text>Message</Text></Cell>
          <Cell><Text>Action</Text></Cell>
        </Head>
        <Row>
          <Cell>
            <Tooltip text="15 Nov 2023 at 7AM"><Text>In 5 days</Text></Tooltip>
          </Cell>
          <Cell>
            <Text>Make sure that we say something meaningful here</Text>
          </Cell>
          <Cell><Button icon="editor-remove" /></Cell>
        </Row>
        <Row>
          <Cell>
            <Tooltip text="15 Nov 2023 at 7AM"><Text>In 5 days</Text></Tooltip>
          </Cell>
          <Cell>
            <Text>(No Message)</Text>
          </Cell>
          <Cell><Button icon="editor-remove" /></Cell>
        </Row>
        <Row>
          <Cell>
            <Tooltip text="15 Nov 2023 at 7AM"><Text>In 5 days</Text></Tooltip>
          </Cell>
          <Cell>
            <Text>Short message here</Text>
          </Cell>
          <Cell><Button icon="editor-remove" /></Cell>
        </Row>
      </Table>
      )}
      {reminders === undefined && (
        <Text>Loading your reminders...</Text>
      )}
      <Text>Your timezone: <Link href="https://rmassaioli-development.atlassian.net/secure/ViewPersonalSettings.jspa">Sydney/Australia</Link></Text>
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
