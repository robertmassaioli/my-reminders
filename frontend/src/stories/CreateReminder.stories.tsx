import styled from 'styled-components';
import React from 'react';
// also exported from '@storybook/react' if you can deal with breaking changes in 6.1
import { Story, Meta } from '@storybook/react/types-6-0';

import { noOp } from './data';
import { ReminderCreateDialog, ReminderCreateDialogProps } from '../ReminderCreateDialog';

export default {
  title: 'Create Reminder',
  component: ReminderCreateDialog,
  argTypes: {
     onCreate: { action: 'create' },
     onCancel: { action: 'cancel' }
   }
} as Meta;

const BlackBack = styled.div`
   background-color: black;
   margin: auto;
`;

const Template: Story<ReminderCreateDialogProps> = (args) => (
   <BlackBack>
      <ReminderCreateDialog {...args} />
   </BlackBack>
);

export const DefaultView = Template.bind({});
DefaultView.args = {

};