import React from 'react';
// also exported from '@storybook/react' if you can deal with breaking changes in 6.1
import { Story, Meta } from '@storybook/react/types-6-0';

import { AllRemindersView, AllRemindersViewProps } from '../AllRemindersView';
import { manyReminders, noOp } from './data';

export default {
  title: 'All Reminders View',
  component: AllRemindersView,
} as Meta;

const Template: Story<AllRemindersViewProps> = (args) => <AllRemindersView {...args} />;

const msPerMinute = 60 * 1000;
const msPerHour = msPerMinute * 60;
const msPerDay = msPerHour * 24;

export const LoadingView = Template.bind({});
LoadingView.args = {
    hostBaseUrl: 'https://your-domain.atlassian.net',
    onDelete: noOp
};

export const EmptyView = Template.bind({});
EmptyView.args = {
    hostBaseUrl: 'https://your-domain.atlassian.net',
    reminders: [],
    onDelete: noOp
};

export const DefaultView = Template.bind({});
DefaultView.args = {
    hostBaseUrl: 'https://your-domain.atlassian.net',
    reminders: manyReminders,
    onDelete: noOp
};

/*


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
*/