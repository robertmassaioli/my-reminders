# Proposal: Migrate to `@forge-clients` for Type-Safe Jira API Calls

**Date:** 2026-04-30  
**Status:** Draft  
**Scope:** Backend resolvers in `src/resolvers/`

---

## Background

The My Reminders app currently makes Jira REST API calls using the raw `@forge/api` pattern:

```js
import api, { route } from "@forge/api";

const notifyResponse = await api.asApp().requestJira(route`/rest/api/3/issue/${reminder.issueId}/notify`, {
  method: 'POST',
  headers: { 'Content-type': 'application/json', ... },
  body: JSON.stringify(jsonBody)
});
if (!notifyResponse.ok) {
  throw new Error(`... ${notifyResponse.status} - ${await notifyResponse.text()}`);
}
```

This pattern has several pain points:

- **No type safety** — parameters, request bodies, and response shapes are all `any`
- **Manual error handling** — every call requires checking `.ok` and reading `.text()` manually
- **No autocompletion** — the API path is a raw string template; typos are silent until runtime
- **Boilerplate** — headers, JSON serialisation, and status checks are repeated everywhere

The [`@forge-clients`](https://robertmassaioli.github.io/forge-clients/) library (authored by robertmassaioli, the same developer as this app) was designed to solve exactly these problems. It provides generated, type-safe clients for the Jira and Confluence REST APIs that work natively with Forge's auth model.

---

## Current `@forge/api` Usage in This Codebase

Only **one file** currently calls the Jira REST API directly:

| File | API call | Auth context |
|---|---|---|
| `src/resolvers/sendReminders.js` | `POST /rest/api/3/issue/{issueId}/notify` | `asApp()` |

The other resolvers (`issueGlance.js`, `yourReminders.js`, `dashboardGadget.js`, `reminderPersistence.js`) only use `@forge/kvs` for storage and `@forge/events` for queuing — no direct Jira REST calls.

`issueGlance.js` also imports `webTrigger` from `@forge/api`, but this is a Forge platform utility (not a Jira REST call) and is **not in scope** for this migration.

---

## Proposed Change

### Installation

Add the two required packages to `my-reminders/package.json`:

```bash
npm install @forge-clients/core @forge-clients/jira
```

`@forge/api` remains as a peer dependency (required by `@forge-clients/core` at runtime) and is still needed for `webTrigger` in `issueGlance.js`.

### Migrating `sendReminders.js`

The only Jira API call in the codebase is the issue notification in `sendExpiredReminder`. Here is what the migration looks like:

**Before:**

```js
import api, { route } from "@forge/api";

const notifyResponse = await api.asApp().requestJira(route`/rest/api/3/issue/${reminder.issueId}/notify`, {
  method: 'POST',
  headers: {
    'Content-type': 'application/json',
    'Accept': 'application/json',
    'x-atlassian-force-account-id': 'true'
  },
  body: JSON.stringify(jsonBody)
});
if (!notifyResponse.ok) {
  throw new Error(`Did not send reminder for user ${reminder.userAaid} for issueId ${reminder.issueId}: ${notifyResponse.status} - ${await notifyResponse.text()}`);
}
```

**After:**

```js
import { ForgeFunctionAdapter, asApp } from '@forge-clients/core';
import { notify } from '@forge-clients/jira/v3';

const adapter = new ForgeFunctionAdapter({ product: 'jira' });

await notify(asApp(adapter), {
  path: { issueIdOrKey: String(reminder.issueId) },
  body: {
    subject,
    textBody,
    htmlBody,
    to: {
      users: [{ accountId: reminder.userAaid }]
    }
  }
});
```

Key improvements:
- The request body is fully typed — `NotificationRecipients`, `Notification`, etc. from `@forge-clients/jira/v3`
- HTTP error responses automatically throw typed errors (`ForbiddenError`, `NotFoundError`, `ForgeApiError`) instead of requiring manual `.ok` checks
- No manual header wrangling — the adapter handles `Content-Type`, auth, and `x-atlassian-force-account-id`
- The existing `try/catch` in `sendExpiredReminder` already handles errors gracefully, so typed errors slot straight in

### Error Handling

The existing `catch` block in `sendExpiredReminder` logs and swallows errors. With `@forge-clients` we can make it more specific at no extra cost:

```js
import { ForgeApiError, NotFoundError } from '@forge-clients/core';

} catch (e) {
  if (e instanceof NotFoundError) {
    // Issue was deleted — safe to remove the reminder without retrying
    console.warn(`Issue ${reminder.issueId} no longer exists; removing reminder ${reminderKey}`);
    await kvs.entity('reminder').delete(reminderKey);
  } else if (e instanceof ForgeApiError) {
    console.error(`API error sending reminder ${reminderKey}: ${e.statusCode} ${e.message}`);
    // TODO: throw to trigger queue retry logic
  } else {
    console.error(`Unexpected error sending reminder ${reminderKey}: ${e}`);
  }
}
```

This is optional but is a natural improvement that the typed errors enable.

---

## Scope Summary

| File | Change required | Notes |
|---|---|---|
| `src/resolvers/sendReminders.js` | **Yes** — replace `api.asApp().requestJira(...)` | Only Jira REST call in the codebase |
| `src/resolvers/issueGlance.js` | **No** | Only uses `@forge/kvs` and `webTrigger` |
| `src/resolvers/yourReminders.js` | **No** | Only uses `@forge/kvs` |
| `src/resolvers/reminderPersistence.js` | **No** | Only uses `@forge/kvs` |
| `src/resolvers/dashboardGadget.js` | **No** | Only uses `@forge/kvs` |
| `src/index.jsx` | **No** | Only uses `@forge/kvs` and `@forge/events` |
| `package.json` | **Yes** — add two new deps | `@forge-clients/core`, `@forge-clients/jira` |

---

## What We Are Not Changing

- **Storage layer** (`@forge/kvs`) — `@forge-clients` is for REST API calls only; KVS calls are unaffected
- **Frontend** — `@forge-clients` targets Forge Function (resolver) and Container environments. The frontend already uses `@forge/bridge` `invoke()` to talk to resolvers; no frontend changes needed
- **Queue / events** (`@forge/events`) — out of scope
- **`webTrigger`** — this is a Forge platform API, not a Jira REST call; stays as-is

---

## Risks and Mitigations

| Risk | Likelihood | Mitigation |
|---|---|---|
| `notify` endpoint not present in `@forge-clients/jira/v3` spec | Low | The Jira v3 client covers 621 endpoints including notifications; verify with TypeScript before shipping |
| `issueId` vs `issueKey` — the current code uses `issueId` (numeric) as the path param | Low | Jira's notify endpoint accepts both; `String(reminder.issueId)` works fine |
| Bundle size increase | Negligible | `@forge-clients` is tree-shakeable; only `notify` and its types will be included |
| Breaking change in notify request body shape | Low | Body is typed; TypeScript will flag any mismatch at compile time |

---

## Recommended Next Steps

1. **Install packages** — `npm install @forge-clients/core @forge-clients/jira` in `my-reminders/`
2. **Migrate `sendReminders.js`** — replace the raw `requestJira` call with `notify()` from `@forge-clients/jira/v3`
3. **Verify types compile** — run `npx tsc --noEmit` (or equivalent lint step) to confirm the notify body matches the generated types
4. **Deploy to dev** — `npm run deploy:dev` and use the webtrigger to exercise the reminder-sending path end-to-end
5. **Remove the `import api from "@forge/api"` line** from `sendReminders.js` — `webTrigger` import in `issueGlance.js` means `@forge/api` stays in `package.json`, so this is just a cleanup within the one file
