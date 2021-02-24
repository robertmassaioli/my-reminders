import React from 'react';
// also exported from '@storybook/react' if you can deal with breaking changes in 6.1
import { Story, Meta } from '@storybook/react/types-6-0';

import { IssueView, IssueViewProps } from '../IssueView';
import { IssueViewActionsProps } from '../IssueViewActions';
import { noOp, todayAdded } from './data';

export default {
  title: 'Issue View',
  component: IssueView,
  argTypes: { onReminderDeleted: { action: 'deleted' }}
} as Meta;

const Template: Story<IssueViewProps & IssueViewActionsProps> = (args) => <IssueView {...args} />;

 const defaultProps: Partial<IssueViewActionsProps & IssueViewProps> = {
   showUpgradeWarning: false,
   onTomorrow: noOp,
   onInAWeek: noOp,
   onInAMonth: noOp,
   onAddReminder: noOp
 };

export const LoadingView = Template.bind({});
LoadingView.args = {
   ...defaultProps
};
export const EmptyView = Template.bind({});
EmptyView.args = {
   ...defaultProps,
   reminders: []
};
export const UpgradeWarning = Template.bind({});
UpgradeWarning.args = {
   ...defaultProps,
   showUpgradeWarning: true,
   reminders: []
};
export const OneReminder = Template.bind({});
OneReminder.args = {
   reminders: [{
      id: 1234,
      message: 'A simple reminder, with a simple message.',
      expiresAt: todayAdded(4, 'days')
  }]
};
export const MultipleReminders = Template.bind({});
MultipleReminders.args = {
   reminders: [{
      id: 1234,
      message: 'A simple reminder, with a simple message.',
      expiresAt: todayAdded(4, 'hours')
  }, {
      id: 1234,
      message: 'A simple reminder, with a simple message.',
      expiresAt: todayAdded(4, 'days')
  }, {
      id: 1234,
      message: 'A simple reminder, with a simple message.',
      expiresAt: todayAdded(8, 'days')
  }]
};