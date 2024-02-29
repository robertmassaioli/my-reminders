import { requestJira } from '@forge/bridge';

export async function getSiteInfo() {
  const response = await requestJira(`/rest/api/3/serverInfo`, {
    headers: {
      'Content-type': 'application/json',
      'Accept': 'application/json'
    }
  });
  return await response.json();
}