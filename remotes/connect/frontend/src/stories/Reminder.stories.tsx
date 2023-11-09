import React from 'react';
// also exported from '@storybook/react' if you can deal with breaking changes in 6.1
import { Story, Meta } from '@storybook/react/types-6-0';

import { Reminder, ReminderProps } from '../Reminder';
import { todayAdded } from './data';

export default {
  title: 'Reminder',
  component: Reminder,
  argTypes: { onDelete: { action: 'delete' }}
} as Meta;

const Template: Story<ReminderProps> = (args) => <Reminder {...args} />;

export const SingleReminder = Template.bind({});
SingleReminder.args = {
   reminder: {
      id: 1234,
      message: 'A single reminder for you.',
      expiresAt: todayAdded(5, 'days')
  }
};
export const LongDescription = Template.bind({});
LongDescription.args = {
   reminder: {
      id: 1234,
      message: 'This is the reminder that never ends, It just goes on and on my friends, Some people \
      started writing it, not knowing what it was, And they\'ll continue writing it forever just because, \
      This is the reminder that never ends, It just goes on and on my friends, Some people started writing \
      it, not knowing what it was, And they\'ll continue writing it forever just because...',
      expiresAt: todayAdded(3, 'days')
  }
};