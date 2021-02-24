import React from 'react';
// also exported from '@storybook/react' if you can deal with breaking changes in 6.1
import { Story, Meta } from '@storybook/react/types-6-0';

import { RemindersList, RemindersListProps } from '../RemindersList';
import { manyReminders, noOp } from './data';

export default {
  title: 'Reminders List',
  component: RemindersList,
} as Meta;

const Template: Story<RemindersListProps> = (args) => <RemindersList {...args} />;

const msPerMinute = 60 * 1000;
const msPerHour = msPerMinute * 60;
const msPerDay = msPerHour * 24;

const defaultProps = {
   hostBaseUrl: 'https://your-domain.atlassian.net',
   onChange: noOp
 };

export const LoadingView = Template.bind({});
LoadingView.args = {
   ...defaultProps,
};
export const EmptyView = Template.bind({});
EmptyView.args = {
   ...defaultProps,
   reminders: []
};
export const OneReminder = Template.bind({});
OneReminder.args = {
   ...defaultProps,
   reminders: [{
      id: 12345,
      key: 'DAC-123',
      summary: 'An issue to behold',
      expiresAt: new Date(new Date().getTime() + msPerDay * 4),
      message: 'Don\'t forget to deal with this issue later!'
  }]
}
export const MultipleReminders = Template.bind({});
MultipleReminders.args = {
   ...defaultProps,
   reminders: manyReminders
}