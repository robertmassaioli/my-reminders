import moment from "moment";

const msPerMinute = 60 * 1000;
const msPerHour = msPerMinute * 60;
export const msPerDay = msPerHour * 24;

export const noOp = () => {
  // no-op
};

export const manyReminders = [{
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

/**
 * Necessary for https://github.com/storybookjs/storybook/issues/12208
 * @param amount
 * @param unit
 */
export function todayAdded(amount?: moment.DurationInputArg1, unit?: moment.DurationInputArg2): moment.Moment {
  return moment().add(amount, unit);
}