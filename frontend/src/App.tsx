import * as React from 'react';
import { Reminder } from './Reminder';
import { IssueViewActions } from './IssueViewActions';

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
      <div className="App">
        <IssueViewActions onAddReminder={noOp} onTomorrow={noOp} onInAWeek={noOp} onInAMonth={noOp}/>
        
        <div className="noReminders"><p>No upcoming reminders</p></div>

        <div className="reminders">
          {this.exampleReminders}
        </div>
      </div>
    );
  }
}

export default App;
