import * as React from 'react';
import { storiesOf, Story } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { Reminder } from '../Reminder';
import { IssueViewActions, IssueViewActionsProps } from '../IssueViewActions';
import { ReminderCreateDialog } from '../ReminderCreateDialog';
import { RemindersList } from '../RemindersList';
import { AllRemindersView } from '../AllRemindersView';
import { IssueView } from '../IssueView';
import { DocsPage } from '../DocsPage';
import * as moment from 'moment';
import styled from 'styled-components';

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

  const deleteAction = action('Reminder delete clicked');

  const BlackBack = styled.div`
    background-color: black;
    margin: auto;
  `;

  storyCreator('Issue view')
    .add('Loading view', () => (
        <IssueView
            showUpgradeWarning={false}
            reminders={undefined}
            onReminderDeleted={deleteAction}
            {...issueViewActionsProps}
        />
    ))
    .add('Empty view', () => (
        <IssueView
            showUpgradeWarning={false}
            reminders={[]}
            onReminderDeleted={deleteAction}
            {...issueViewActionsProps}
        />
    ))
    .add('Empty view with upgrade warning', () => (
        <IssueView
            showUpgradeWarning={true}
            reminders={[]}
            onReminderDeleted={deleteAction}
            {...issueViewActionsProps}
        />
    ))
    .add('One reminder', () => (
        <IssueView
            showUpgradeWarning={false}
            reminders={[{
                id: 1234,
                message: 'A simple reminder, with a simple message.',
                expiresAt: moment().add(4, 'days')
            }]}
            onReminderDeleted={deleteAction}
            {...issueViewActionsProps}
        />
    ))
    .add('Multiple reminders', () => (
        <IssueView
            showUpgradeWarning={false}
            reminders={[{
                id: 1234,
                message: 'A simple reminder, with a simple message.',
                expiresAt: moment().add(4, 'hours')
            }, {
                id: 1234,
                message: 'A simple reminder, with a simple message.',
                expiresAt: moment().add(4, 'days')
            }, {
                id: 1234,
                message: 'A simple reminder, with a simple message.',
                expiresAt: moment().add(8, 'days')
            }]}
            onReminderDeleted={deleteAction}
            {...issueViewActionsProps}
        />
    ));

  storyCreator('All reminders view')
    .add('Loading view', () => (
        <AllRemindersView
            hostBaseUrl="https://your-domain.atlassian.net"
            onDelete={noOp}
        />
    ))
    .add('Empty view', () => (
        <AllRemindersView
            hostBaseUrl="https://your-domain.atlassian.net"
            reminders={[]}
            onDelete={noOp}
        />
    ))
    .add('Default view', () => (
        <AllRemindersView
            hostBaseUrl="https://your-domain.atlassian.net"
            reminders={manyReminders}
            onDelete={noOp}
        />
    ));

  storyCreator('Reminders List')
    .add('Loading view', () => (
        <RemindersList {...remindersListProps} />
    ))
    .add('Empty view', () => (
        <RemindersList reminders={[]} {...remindersListProps} />
    )).add('One reminder', () => (
        <RemindersList
            reminders={[{
                id: 12345,
                key: 'DAC-123',
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
        <BlackBack>
            <ReminderCreateDialog onCreate={noOp} onCancel={noOp} />
        </BlackBack>
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
            expiresAt: moment().add(5, 'days')
        }}
        onDelete={deleteAction}
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
                expiresAt: moment().add(3, 'days')
            }}
            onDelete={deleteAction}
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
                        expiresAt: moment().add(time, 'milliseconds')
                    }}
                    onDelete={deleteAction}
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
                    expiresAt: moment().add(3, 'days')
                }}
                onDelete={deleteAction}
            />
            <Reminder
                reminder={{
                    id: 1,
                    message: 'A reminder that is repeated a lot.',
                    expiresAt: moment().add(3, 'days')
                }}
                onDelete={deleteAction}
            />
        </div>
    ));

  // tslint:disable:align
  const simpleMdContent = `
# Hello there

This is your captain speaking. Some points:

 - Nitwit
 - Odment
 - Blubber

And some code:

    1 + 2 = 3

There we go.
  `;
  // tslint:enable:align

  storyCreator('Docs page')
    .add('Default view', () => (
        <DocsPage mdContent={simpleMdContent} location="/docs/home" />
    ));
}