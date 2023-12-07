export function toDateOutput(input) {
  const expiry = input.clone();
  const dateOutput = expiry.format('D MMM YYYY');
  const hourOutput = expiry.format('h');
  const meridian = expiry.format('A');
  expiry.add(1, 'hour');
  const nextHourOutput = expiry.format('h');

  return `${dateOutput} at ${hourOutput}-${nextHourOutput}${meridian}`;
}