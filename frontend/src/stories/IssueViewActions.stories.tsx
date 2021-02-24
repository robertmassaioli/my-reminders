import React from 'react';
// also exported from '@storybook/react' if you can deal with breaking changes in 6.1
import { Story, Meta } from '@storybook/react/types-6-0';

import { IssueView, IssueViewProps } from '../IssueView';
import { IssueViewActions, IssueViewActionsProps } from '../IssueViewActions';

export default {
  title: 'Issue View Actions',
  component: IssueView,
  argTypes: {
     onAddReminder: { action: 'onAddReminder' },
     onTomorrow: { action: 'onTomorrow' },
     onInAWeek: { action: 'onInAWeek' },
     onInAMonth: { action: 'onInAMonth' }
   }
} as Meta;

const Template: Story<IssueViewActionsProps> = (args) => <IssueViewActions {...args} />;

export const DefaultView = Template.bind({});
DefaultView.args = {};

export const ActionInProgress = Template.bind({});
ActionInProgress.args = {
   statusIndicator: 'actionInProgress'
};

export const ErrorState = Template.bind({});
ErrorState.args = {
   statusIndicator: 'error'
};