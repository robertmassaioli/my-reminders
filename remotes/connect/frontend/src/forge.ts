export function isForgeConnectIframe(): boolean {
  // Get url query string
  const query = window.location.search.substring(1);

  // Split the query string into individual parameters
  const vars = query.split("&");

  // Iterate over parameters
  for (let i=0;i<vars.length;i++) {
      // Split parameter into key-value pair
      let pair = vars[i].split("=");

      // Check if key is "forge" and value is "true"
      if (pair[0] === "forge" && pair[1] === "true") {
          return true;
      }
  }

  // Return false if "forge" parameter is not found or not "true"
  return false;
}