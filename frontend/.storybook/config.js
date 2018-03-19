import { configure } from '@storybook/react';
import '@atlaskit/css-reset';
import { compileStories } from '../src/stories';

function loadStories() {
  compileStories();
}

configure(loadStories, module);
