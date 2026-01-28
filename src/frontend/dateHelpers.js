import moment from 'moment-timezone/builds/moment-timezone-with-data';

export function toDateOutput(input) {
  const expiry = input.clone();
  const dateOutput = expiry.format('D MMM YYYY');
  const timeOutput = expiry.format('h:mm A'); // Show exact time with minutes

  return `${dateOutput} at ${timeOutput}`;
}

function getRandomTimeInHour(baseHour) {
  const randomMinute = Math.floor(Math.random() * 60);
  return { hour: baseHour, minute: randomMinute };
}

// Time Calculator Functions
export function getTomorrowMorning() {
  const randomTime = getRandomTimeInHour(6);
  return moment()
    .add(1, 'day')
    .hour(randomTime.hour)
    .minute(randomTime.minute)
    .second(0);
}

export function getIn24Hours() {
  return moment().add(24, 'hours').second(0);
}

export function getNextMonday() {
  const randomTime = getRandomTimeInHour(6);
  return moment()
    .day(1) // Monday
    .add(1, 'week')
    .hour(randomTime.hour)
    .minute(randomTime.minute)
    .second(0);
}

export function getInSevenDays() {
  return moment().add(7, 'days').second(0);
}

export function getInOneMonth() {
  const randomTime = getRandomTimeInHour(6);
  return moment()
    .add(1, 'month')
    .hour(randomTime.hour)
    .minute(randomTime.minute)
    .second(0);
}

export function getFirstDayOfNextQuarter() {
  const now = moment();
  const currentQuarter = Math.ceil((now.month() + 1) / 3);
  const nextQuarterMonth = (currentQuarter * 3) % 12;
  const nextQuarterYear = nextQuarterMonth === 0 ? now.year() + 1 : now.year();
  const randomTime = getRandomTimeInHour(6);
  
  return moment()
    .year(nextQuarterYear)
    .month(nextQuarterMonth)
    .date(1)
    .hour(randomTime.hour)
    .minute(randomTime.minute)
    .second(0);
}

export function getInThreeMonths() {
  const randomTime = getRandomTimeInHour(6);
  return moment()
    .add(3, 'months')
    .hour(randomTime.hour)
    .minute(randomTime.minute)
    .second(0);
}

export function getEndOfDay() {
  const randomTime = getRandomTimeInHour(16); // 4 PM hour
  return moment()
    .hour(randomTime.hour)
    .minute(randomTime.minute)
    .second(0);
}

export function getEndOfWeek() {
  const randomTime = getRandomTimeInHour(16); // 4 PM hour
  return moment()
    .day(5) // Friday
    .hour(randomTime.hour)
    .minute(randomTime.minute)
    .second(0);
}

export function getInOneYear() {
  const randomTime = getRandomTimeInHour(6);
  return moment()
    .add(1, 'year')
    .hour(randomTime.hour)
    .minute(randomTime.minute)
    .second(0);
}