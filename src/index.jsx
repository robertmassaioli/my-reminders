// New logic, can just be a .js file
export { handler } from './resolvers';
//TODO Old file for UI Kit 1

import api, { route, storage } from "@forge/api";
import ForgeUI, { render, Text, Fragment, GlobalPage, useState, IssueContext, ButtonSet, Button, Heading, Table, Head, Row, Cell, Link, DateLozenge, ModalDialog, Tooltip, useProductContext} from '@forge/ui';

const fetchProjects = async () => {
  const res = await api.asApp().requestJira(route`/rest/api/3/project`, {
    headers: {
      'Accept': 'application/json'
    }
  });

  const data = await res.json();
  return data;
};

const App = () => {
  const [projects] = useState(async () => await fetchProjects());

  return (
    <Fragment>
      <Text>Hello world! This is a General Page in the My Reminders App. This is Forge UI. It works! Boom. A Connect on Forge App!</Text>
      {projects.map(project => (<Text>{project.name}</Text>))}
    </Fragment>
  );
};

export const run = render(
  <GlobalPage>
    <App/>
  </GlobalPage>
);

async function loadReminders(viewContext) {
  const result = await storage
    .entity('reminder')
    .query()
    .index("by-aaid-and-issue-id", {
      partition: [viewContext.userAaid, viewContext.issueId]
    })
    .getMany();

  return result.results;
}

async function createReminderForTomorrow(viewContext) {
  const now = new Date(); // TODO Should be for the next day
  await storage.entity('reminder').set(`${now.getTime()}`, {
    issueId: viewContext.issueId,
    issueKey: 'todo',
    originalIssueKey: 'todo',
    issueSummary: 'todo',
    originalIssueSummary: 'todo',
    message: undefined,
    date: now.getTime(),
    day: now.toDateString(),
    userAaid: viewContext.userAaid,
    sendAttempts: 0
  });

  return await loadReminders(viewContext);
}


const MyRemindersIssueView = () => {
  const context = useProductContext();
  const [isAddReminderOpen, setAddReminderOpen] = useState(false);
  // TODO If there is no issue id, or account id, we should handle that
  const viewContext = {
    issueId: context.platformContext.issueId,
    issueKey: context.platformContext.issueKey,
    userAaid: context.accountId
  };
  console.log(`${context.issueId}, ${context.accountId}`);
  const [reminders, setReminders] = useState(async () => await loadReminders(viewContext));

  async function refreshReminders() {
    setReminders(await loadReminders(viewContext));
  }


  return (
    <Fragment>
      <Heading size="medium">Add reminder</Heading>
      <ButtonSet>
        <Button text="Tomorrow" onClick={async () => setReminders(await createReminderForTomorrow(viewContext))} />
        <Button text="In a Week" />
        <Button text="Select a time..." onClick={() => setAddReminderOpen(true)}/>
      </ButtonSet>
      <Heading size="medium">Your reminders</Heading>
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
      <Text>Your timezone: <Link href="https://rmassaioli-development.atlassian.net/secure/ViewPersonalSettings.jspa">Sydney/Australia</Link></Text>
      {isAddReminderOpen && (
        <ModalDialog header="Add a Reminder" onClose={() => setAddReminderOpen(false)}>
          <Text>Hello!</Text>
        </ModalDialog>
      )}
    </Fragment>
  );
}

/**
 * In order to make the viewIssueGlance work we need to:
 *  - Talk securely with the connect remote. Does this require CustomUI and Forge Remote?
 *  - Can we make these reminders move to Forge Storage instead?
 *    - Forge storage can support 240kb per key. No limits on the number of keys. I could work with this perhaps...
 *    - Don't need the purged_tenant, tenant or installed_migrations tables
 *  - Don't need https://developer.atlassian.com/platform/forge/manifest-reference/modules/scheduled-trigger/
 *  - How do we send all of the reminders within the execution time limit? Need to use forge events?
 */
export const viewIssueGlance = render(
  <IssueContext>
    <MyRemindersIssueView />
  </IssueContext>
);