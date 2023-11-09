import api, { route } from "@forge/api";
import ForgeUI, { render, Text, Fragment, GlobalPage, useState} from '@forge/ui';

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