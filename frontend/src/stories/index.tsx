import * as React from 'react';
import { storiesOf, Story } from '@storybook/react';
import { Reminder } from '../Reminder';
import { IssueViewActions, IssueViewActionsProps } from '../IssueViewActions';
import { ReminderCreateDialog } from '../ReminderCreateDialog';
import { RemindersList } from '../RemindersList';
import { AllRemindersView } from '../AllRemindersView';
import { IssueView } from '../IssueView';

type partialStoriesOf = (name: string) => Story;

export function compileStories() {
  const localStoryCreator: partialStoriesOf = (name: string) => storiesOf(name, module);
  stories(localStoryCreator);
}

export default function stories(storyCreator: partialStoriesOf) {
  const msPerMinute = 60 * 1000;
  const msPerHour = msPerMinute * 60;
  const msPerDay = msPerHour * 24;
  const noOp = () => {
      // no-op
  };

  const issueViewActionsProps: IssueViewActionsProps = {
    onTomorrow: noOp,
    onInAWeek: noOp,
    onInAMonth: noOp,
    onAddReminder: noOp
  };

  const remindersListProps = {
    hostBaseUrl: 'https://your-domain.atlassian.net',
    onChange: noOp
  };

  const manyReminders = [{
        id: 12345, 
        key: 'DAC-123', 
        email: 'example@test.test', 
        summary: 'An issue to behold',
        expiresAt: new Date(new Date().getTime() + msPerHour * 4),
        message: 'Don\'t forget to deal with this issue later!'
    }, {
        id: 12346, 
        key: 'MR-983482', 
        email: 'example@test.test', 
        summary: 'The second best issue you have ever seen.',
        expiresAt: new Date(new Date().getTime() + msPerDay * 4),
    }, {
        id: 12347, 
        key: 'SHPXXXVI-1234', 
        email: 'example@test.test', 
        summary: 'The mightiest issue in the world',
        expiresAt: new Date(new Date().getTime() + msPerDay * 8),
        message: 'There is something strange, in your neighbourhood.'
    }, {
        id: 12348, 
        key: 'DAC-10', 
        email: 'example@test.test', 
        summary: 'A strange issue to behold',
        expiresAt: new Date(new Date().getTime() + msPerDay * 12),
        message: `A really ${'super '.repeat(20)} long message!`
    }];

  storyCreator('Issue view')
    .add('Empty view', () => (
        <IssueView reminders={[]} {...issueViewActionsProps} />
    ))
    .add('One reminder', () => (
        <IssueView 
            reminders={[{
                id: 1234,
                message: 'A simple reminder, with a simple message.',
                expiresAt: new Date(new Date().getTime() + msPerDay * 4),
                timezone: 'Australia/Sydney'
            }]}
            {...issueViewActionsProps}
        />
    ))
    .add('Multiple reminders', () => (
        <IssueView 
            reminders={[{
                id: 1234,
                message: 'A simple reminder, with a simple message.',
                expiresAt: new Date(new Date().getTime() + msPerHour * 4),
                timezone: 'Australia/Sydney'
            }, {
                id: 1234,
                message: 'A simple reminder, with a simple message.',
                expiresAt: new Date(new Date().getTime() + msPerDay * 4),
                timezone: 'Australia/Sydney'
            }, {
                id: 1234,
                message: 'A simple reminder, with a simple message.',
                expiresAt: new Date(new Date().getTime() + msPerDay * 8),
                timezone: 'Australia/Sydney'
            }]}
            {...issueViewActionsProps}
        />
    ));

  storyCreator('All reminders view')
    .add('Default view', () => (
        <AllRemindersView 
            hostBaseUrl="https://your-domain.atlassian.net"
            reminders={manyReminders}
            onUpdateEmail={noOp}
            onDelete={noOp}
        />
    ));

  storyCreator('Reminders List')
    .add('Empty view', () => (
        <RemindersList reminders={[]} {...remindersListProps} />
    )).add('One reminder', () => (
        <RemindersList 
            reminders={[{
                id: 12345, 
                key: 'DAC-123', 
                email: 'example@test.test', 
                summary: 'An issue to behold',
                expiresAt: new Date(new Date().getTime() + msPerDay * 4),
                message: 'Don\'t forget to deal with this issue later!'
            }]}
            {...remindersListProps}
        />
    )).add('Multiple reminders', () => (
        <RemindersList 
            reminders={manyReminders}
            {...remindersListProps}
        />
    ));

  storyCreator('Create reminder')
    .add('Default view', () => (
        <ReminderCreateDialog onCreate={noOp} onCancel={noOp} />
    ));

  storyCreator('Issue View Actions')
    .add('Default view', () => (
      <IssueViewActions {...issueViewActionsProps} />
    ))
    .add('Action in progress', () => (
      <IssueViewActions 
        statusIndicator="actionInProgress" 
        {...issueViewActionsProps}
      />
    ))
    .add('Error state', () => (
      <IssueViewActions 
        statusIndicator="error" 
        {...issueViewActionsProps} 
      />
    ));

  storyCreator('Reminder')
    .add('Single reminder', () => (
      <Reminder
        reminder={{
            id: 1234,
            message: 'A single reminder for you.',
            expiresAt: new Date(new Date().getTime() + msPerDay * 5),
            timezone: 'Australia/Sydney'
        }}
      />
    ))
    .add('Long description', () => (
        <Reminder
            reminder={{
                id: 1234,
                message: 'This is the reminder that never ends, It just goes on and on my friends, Some people \
                started writing it, not knowing what it was, And they\'ll continue writing it forever just because, \
                This is the reminder that never ends, It just goes on and on my friends, Some people started writing \
                it, not knowing what it was, And they\'ll continue writing it forever just because...',
                expiresAt: new Date(new Date().getTime() + msPerDay * 5),
                timezone: 'Australia/Sydney'
            }}
        /> 
    ))
    .add('Multiple reminders', () => {
        const times = [
            0, 
            msPerMinute * 2, 
            msPerHour * 2,
            msPerDay * 2,
            msPerDay * 2 * 30,
            msPerDay * 365 * 2,
            msPerDay * 365 * 20

        ];
        return times.map(time => {
            return (
                <Reminder
                    key={time}
                    reminder={{
                        id: 1,
                        message: 'A reminder that is repeated a lot.',
                        expiresAt: new Date(new Date().getTime() + time),
                        timezone: 'Australia/Sydney'
                    }}
                />
            );
        });
    })
    .add('Different timezones', () => (
        <div>
            <Reminder
                reminder={{
                    id: 1,
                    message: 'A reminder that is repeated a lot.',
                    expiresAt: new Date(new Date().getTime() + msPerDay * 3),
                    timezone: 'Australia/Sydney'
                }}
            />
            <Reminder
                reminder={{
                    id: 1,
                    message: 'A reminder that is repeated a lot.',
                    expiresAt: new Date(new Date().getTime() + msPerDay * 3),
                    timezone: 'Australia/Sydney'
                }}
            />
        </div>
    ));
}