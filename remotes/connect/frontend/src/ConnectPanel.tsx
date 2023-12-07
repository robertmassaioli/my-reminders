import React from 'react';
import { Route, Switch } from 'react-router-dom';
import { loadPageContext, PageContext } from './page-context';
import Spinner from '@atlaskit/spinner';
import { IssueViewContainer } from './IssueViewContainer';
import { AllRemindersViewContainer } from './AllRemindersViewContainer';
import { ReminderCreateContainer } from './ReminderCreateContainer';

type ConnectPanelState = {
    pageContext: PageContext | undefined;
    apLoaded: boolean;
  };

export class ConnectPanel extends React.PureComponent<{}, ConnectPanelState> {
  componentWillMount() {
    const pc = loadPageContext();

    this.setState({
      pageContext: pc,
      apLoaded: false
    });

    if (pc) {
      const script = document.createElement('script');
      script.src = `https://connect-cdn.atl-paas.net/all.js`;

      script.onload = () => {
        this.setState(s => {
          return {
            ...s,
            apLoaded: true
          };
        });
      };

      document.getElementsByTagName('head')[0].appendChild(script);
    }
  }

  componentDidUpdate() {
    if (this.state.apLoaded) {
      AP.resize();
    }
  }

  render() {
    const pc = this.state.pageContext;
    if (!pc) {
      return <div>Error state: no page context!</div>;
    } else if (!this.state.apLoaded) {
      return <Spinner delay={100} size="large" />;
    }

    return (
        <Switch>
        <Route
            exact={true}
            path="/panel/jira/reminder/simple"
            render={(props) => <IssueViewContainer {...props} pageContext={pc} />}
        />
        <Route
            exact={true}
            path="/panel/jira/reminders/view"
            render={(props) => <AllRemindersViewContainer {...props} pageContext={pc} />}
        />
        <Route
            exact={true}
            path="/panel/v2/jira/reminder/create"
            render={(props) => <ReminderCreateContainer {...props} pageContext={pc} />}
        />
        </Switch>
    );
  }
}