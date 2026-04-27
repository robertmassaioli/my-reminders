import moment from 'moment-timezone/builds/moment-timezone-with-data';
import {
  toDateOutput,
  getTomorrowMorning,
  getIn24Hours,
  getNextMonday,
  getInSevenDays,
  getInOneMonth,
  getFirstDayOfNextQuarter,
  getInThreeMonths,
  getEndOfDay,
  getEndOfWeek,
  getInOneYear,
} from '../frontend/dateHelpers';

describe('toDateOutput', () => {
  it('formats a moment as "D MMM YYYY at h:mm A"', () => {
    const date = moment('2026-06-05T09:00:00');
    expect(toDateOutput(date)).toBe('5 Jun 2026 at 9:00 AM');
  });

  it('formats PM times correctly', () => {
    const date = moment('2026-12-31T16:30:00');
    expect(toDateOutput(date)).toBe('31 Dec 2026 at 4:30 PM');
  });

  it('does not mutate the input moment', () => {
    const date = moment('2026-06-05T09:00:00');
    const before = date.valueOf();
    toDateOutput(date);
    expect(date.valueOf()).toBe(before);
  });
});

describe('Quick-pick helpers — all return future moments', () => {
  const now = moment();

  it('getTomorrowMorning returns a moment after now', () => {
    expect(getTomorrowMorning().isAfter(now)).toBe(true);
  });

  it('getTomorrowMorning is in hour 6', () => {
    expect(getTomorrowMorning().hour()).toBe(6);
  });

  it('getIn24Hours returns a moment ~24h from now', () => {
    const result = getIn24Hours();
    expect(result.isAfter(now)).toBe(true);
    // Use >=23 to account for milliseconds elapsed between `now` and the call
    expect(result.diff(now, 'hours')).toBeGreaterThanOrEqual(23);
  });

  it('getNextMonday returns a future Monday', () => {
    const result = getNextMonday();
    expect(result.isAfter(now)).toBe(true);
    expect(result.day()).toBe(1); // 1 = Monday
  });

  it('getInSevenDays returns a moment ~7 days from now', () => {
    const result = getInSevenDays();
    expect(result.isAfter(now)).toBe(true);
    // Use >=6 to account for milliseconds elapsed between `now` and the call
    expect(result.diff(now, 'days')).toBeGreaterThanOrEqual(6);
  });

  it('getInOneMonth returns a moment after now', () => {
    expect(getInOneMonth().isAfter(now)).toBe(true);
  });

  it('getFirstDayOfNextQuarter returns the 1st of a future month', () => {
    const result = getFirstDayOfNextQuarter();
    expect(result.isAfter(now)).toBe(true);
    expect(result.date()).toBe(1);
    expect([0, 3, 6, 9]).toContain(result.month()); // Jan, Apr, Jul, Oct
  });

  it('getInThreeMonths returns a moment after now', () => {
    expect(getInThreeMonths().isAfter(now)).toBe(true);
  });

  it('getEndOfDay returns hour 16', () => {
    expect(getEndOfDay().hour()).toBe(16);
  });

  it('getEndOfWeek returns day 5 (Friday)', () => {
    expect(getEndOfWeek().day()).toBe(5);
  });

  it('getInOneYear returns a moment approximately one year from now', () => {
    const result = getInOneYear();
    expect(result.isAfter(now)).toBe(true);
    expect(result.diff(now, 'days')).toBeGreaterThanOrEqual(364);
  });

  it('all helpers return seconds set to 0', () => {
    const helpers = [
      getTomorrowMorning,
      getNextMonday,
      getInOneMonth,
      getFirstDayOfNextQuarter,
      getInThreeMonths,
      getEndOfDay,
      getEndOfWeek,
      getInOneYear,
    ];
    helpers.forEach(fn => {
      expect(fn().second()).toBe(0);
    });
  });
});
