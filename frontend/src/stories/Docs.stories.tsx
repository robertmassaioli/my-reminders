import React from 'react';
// also exported from '@storybook/react' if you can deal with breaking changes in 6.1
import { Story, Meta } from '@storybook/react/types-6-0';

import { DocsPage, DocsPageProps } from '../DocsPage';

export default {
  title: 'Docs',
  component: DocsPage,
  argTypes: { onDelete: { action: 'delete' }}
} as Meta;

const Template: Story<DocsPageProps> = (args) => <DocsPage {...args} />;


// tslint:disable:align
  const mdContent = `
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

export const DefaultView = Template.bind({});
DefaultView.args = {
   mdContent,
   location: '/docs/home'
};