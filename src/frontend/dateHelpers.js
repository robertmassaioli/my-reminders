import moment from "moment";

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

export function getNextQuarter() {
  const randomTime = getRandomTimeInHour(6);
  return moment()
    .add(3, 'months')
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