import * as React from 'react';
import { BrowserRouter as Router, Route, Switch } from 'react-router-dom';
import { ConnectPanel } from './ConnectPanel';
import { DocsPageContainer } from './DocsPageContainer';

class App extends React.Component {

  render() {
    return (
      <Router>
        <div className="App">
          <Switch>
            <Route path="/panel/" component={ConnectPanel} />
            <Route path="/docs/:page" component={DocsPageContainer} />
          </Switch>
        </div>
      </Router>
    );
  }
}

export default App;
