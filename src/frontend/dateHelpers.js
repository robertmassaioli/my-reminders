export function toDateOutput(input) {
  const expiry = input.clone();
  const dateOutput = expiry.format('D MMM YYYY');
  const timeOutput = expiry.format('h:mm A'); // Show exact time with minutes

  return `${dateOutput} at ${timeOutput}`;
}