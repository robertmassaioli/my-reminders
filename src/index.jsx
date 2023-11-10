import api, { route } from "@forge/api";
import ForgeUI, { render, Text, Fragment, GlobalPage, useState, IssueContext} from '@forge/ui';

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
    <Text>HelloWorld!</Text>
  </IssueContext>
);