import * as React from 'react';
import { Reminder } from './Reminder';
import { IssueView } from './IssueView';
import { AllRemindersView } from './AllRemindersView';
import { ReminderCreateDialog } from './ReminderCreateDialog';
import { IssueViewActions } from './IssueViewActions';
import { BrowserRouter as Router, Route } from 'react-router-dom';

class App extends React.Component {
  private exampleReminders: JSX.Element[] = (() => {
    let rows = new Array<JSX.Element>();
    for (let i = 0; i < 10; i++) {
    rows.push((
      <Reminder
        reminder={{
          id: 100,
          message: 'Catch up with Karen about the thing at the place.',
          expiresAt: new Date(new Date().getTime() + Math.random() * 200000000),
          timezone: 'Australia/Sydney'
        }} 
      />
      ));
    }
    return rows;
  })();

  render() {
    const noOp = () => {
      return 0;
    };
    return (
      <Router>
        <Route exact path="/panel/jira/reminder/simple" component={IssueView} />
        <Route exact path="/panel/jira/reminders/view" component={AllRemindersView} /> 
        <Route exact path="/panel/v2/jira/reminder/create" component={ReminderCreateDialog} />
        <div className="App">
          <IssueViewActions onAddReminder={noOp} onTomorrow={noOp} onInAWeek={noOp} onInAMonth={noOp}/>
          
          <div className="noReminders"><p>No upcoming reminders</p></div>

          <div className="reminders">
            {this.exampleReminders}
          </div>
        </div>
      </Router>
    );
  }
}

export default App;
