# My Reminders — 10 Cross-Functional Improvement Proposals

> **Audience:** Engineering team, product owner, and stakeholders.  
> **Purpose:** A prioritised, cross-functional set of improvements spanning correctness, security, UX, observability, documentation, and developer experience.  
> **Date:** April 2026

> ⚠️ **UI Constraint:** This is a Forge app using **UI Kit 2**. All UI must be built exclusively with components from `@forge/react` (e.g. `Text`, `Button`, `TextArea`, `Stack`, `Inline`, `Box`, `Label`, `HelperMessage`, etc.). Standard HTML elements (`<div>`, `<p>`, `<input>`, etc.) and raw React DOM components **will not render** in the Forge sandboxed environment. No direct DOM access, no `className`, no inline styles — use `xcss()` with design tokens for all styling.

---

## 1. 🗑️ Remove Dead `deleteAllReminders` Code & Plan the Feature Properly (Code Quality)

**Area:** Backend / Code Quality  
**Effort:** XS (< 1 hour) — **Done:** dead code removed from `src/resolvers/yourReminders.js`

### Problem
The `deleteAllReminders` resolver handler existed in `src/resolvers/yourReminders.js` but was never wired up to any frontend component — no button, no `invoke()` call, nothing. The handler also referenced an undefined `req` variable, meaning it would have thrown a `ReferenceError` if ever called. It was purely dead, incomplete code with a latent bug.

### Resolution
The broken handler has been **deleted** from `yourReminders.js`. The codebase is now free of this dead code.

### Future Work
If a "Delete all reminders" feature is desired, it should be treated as a deliberate new feature:
- Implement the backend handler correctly (iterate `getYourReminders`, call `deleteReminder` for each).
- Add a "Delete all" button with a confirmation dialog to `viewYourReminders.jsx`.
- Document it in the FAQ.
- Cover it with tests (see Improvement #6).

---

## 2. 🔁 Implement Retry Logic for Failed Reminder Sends (Reliability)

**Area:** Backend / Reliability  
**Effort:** M (1–2 days)

### Problem
In `src/resolvers/sendReminders.js`, the `catch` block swallows errors after logging them:

```js
} catch (e) {
  // TODO throw an error to trigger the retry logic
  console.error(`Error sending: ${reminderKey}: ${e}`);
}
```

If the Jira notification API is temporarily unavailable (rate limit, transient 5xx, etc.), the reminder silently fails and is never retried. The `sendAttempts` field exists in the data model specifically to support retry tracking, but it is never incremented.

### Proposed Fix
- **Re-throw** the error from the catch block so Forge's consumer queue triggers its built-in retry with exponential back-off.
- **Increment `sendAttempts`** before each attempt and write it back to KVS.
- **Set a max-retry threshold** (e.g. 5 attempts). After the threshold, log a structured error and delete the reminder to prevent it being stuck indefinitely.
- Update the FAQ to note that reminders may be delayed up to ~30 minutes in cases of transient platform errors.

---

## 3. 🔒 Add a Message Length Limit Enforcement (Security / Data Integrity)

**Area:** Security / Backend  
**Effort:** S (half a day)

### Problem
There is an explicit `TODO` in `src/resolvers/issueGlance.js` acknowledging that message length is unconstrained:

```js
// TODO make sure that the message is no more than X characters long.
message: blankToUndefined(message),
```

An unbounded message allows a user to store arbitrarily large strings in Forge KVS. Combined with up to 10 reminders per issue per user, this could inflate storage costs, cause downstream email rendering issues, and potentially be abused to stuff large payloads into the system.

### Proposed Fix
- Enforce a **1024-character limit** on `message` in the `createReminder` resolver.
- Return a structured error to the frontend if the limit is exceeded.
- Add a live character counter to the `issueGlance.jsx` custom message input (e.g. `"142 / 1024 characters"`).
- Document the limit in the FAQ under "Using My Reminders".

---

## 4. 📊 Add Structured Logging and Observability (Observability)

**Area:** Backend / Observability  
**Effort:** M (1–2 days)

### Problem
Logging across the resolvers is inconsistent and unstructured:
- `console.info`, `console.warn`, `console.error`, and `console.debug` are used ad-hoc.
- Log messages are plain strings — not machine-parseable.
- There is no correlation between the scheduled trigger (`scheduleExpiryJobs`), the queue push, and the consumer (`sendExpiredReminder`), making it impossible to trace a single reminder's lifecycle end-to-end.
- A commented-out TODO in `sendReminders.js` notes: `// TODO: Move this into a different resolver for observability`.

### Proposed Fix
- Introduce a thin `logger` utility (`src/resolvers/logger.js`) that wraps `console.*` and emits structured JSON: `{ level, message, reminderKey, userAaid, issueId, timestamp }`.
- Assign a **correlation ID** (e.g. the reminder's KVS key) at the point of queuing and carry it through every log line for that reminder.
- Log standardised lifecycle events: `reminder.queued`, `reminder.send.attempt`, `reminder.send.success`, `reminder.send.failure`, `reminder.deleted`.
- This makes Forge's "Download logs" feature (referenced in the FAQ troubleshooting section) actually actionable for both admins and developers.

---

## 5. ⏰ Fix Randomised Quick-Pick Times & Improve UX Consistency (UX / Design)

**Area:** Frontend / UX  
**Effort:** S–M (1 day)

### Problem
In `src/frontend/dateHelpers.js`, every quick-pick helper (e.g. `getTomorrowMorning`, `getNextMonday`, `getInOneMonth`) uses a random minute within the target hour:

```js
function getRandomTimeInHour(baseHour) {
  const randomMinute = Math.floor(Math.random() * 60);
  return { hour: baseHour, minute: randomMinute };
}
```

This means clicking "Tomorrow Morning" might give `6:00 AM` one time and `6:47 AM` another. There are several secondary issues:
- `getEndOfDay()` targets 4 PM (`hour: 16`), which is not "end of day" — it is mid-afternoon. Most users would expect 5 PM or 6 PM.
- `getEndOfWeek()` targets Friday at 4 PM. If today is Saturday or Sunday, `moment().day(5)` returns the *previous* Friday, creating a reminder in the past.
- `getNextMonday()` has the same past-date risk if called on a Monday.

### Proposed Fix
- Replace random minutes with **fixed, predictable times** (e.g. `getTomorrowMorning` → always `9:00 AM`, `getEndOfDay` → always `5:00 PM`).
- Guard `getEndOfWeek()` and `getNextMonday()` to always return a *future* date (add a week if the computed date is in the past).
- Show the computed time in the quick-pick button tooltip so users know exactly when the reminder will fire before clicking.
- Update `getEndOfDay` label from "End of day" to reflect the actual target time if it remains at 4 PM, or change the target to 5 PM.

---

## 6. 🧪 Add an Automated Test Suite (Developer Experience / Quality)

**Area:** Developer Experience / Quality  
**Effort:** L (3–5 days)

### Problem
There are zero automated tests in the repository. The `package.json` has no `test` script. Logic bugs like the `deleteAllReminders` bug and the past-date quick-pick helpers can only be caught by manual testing or code review. The CI pipeline (`bitbucket-pipelines.yml`) runs linting but no unit or integration tests.

### Proposed Fix
- Add **Jest** as a dev dependency and configure it for ESM/JSX (matching the existing `parserOptions` in `.eslintrc`).
- Add a `"test": "jest"` script to `package.json` and include it in the default pipeline step.
- Write unit tests for:
  - `src/frontend/dateHelpers.js` — all quick-pick helpers (verifying outputs are always in the future, times are deterministic).
  - `src/resolvers/reminderPersistence.js` — `deleteReminder` ownership check logic.
  - `src/resolvers/sendReminders.js` — `generateHtmlBody` and `generateTextBody` for moved-issue scenarios.
- Add a test coverage threshold (e.g. 70%) enforced in CI.

---

## 7. 🔄 Update the CI/CD Pipeline to Reflect the Forge Architecture (DevOps)

**Area:** CI/CD / DevOps  
**Effort:** M (1–2 days)

### Problem
`bitbucket-pipelines.yml` is a clear relic of the **pre-Forge Connect era**. The default pipeline builds a Haskell backend and a Node 14 frontend. The master branch builds a Docker image and deploys it via `micros`. None of this applies to the current Forge app. The pipeline:
- Uses `node:14` (the app requires Node 20 per `.nvmrc`).
- Builds and deploys a Haskell service that no longer exists in this repo.
- Does not run `forge deploy` at any stage.
- Does not run ESLint (`npm run lint`) in CI.
- Does not run tests (see Improvement #6).

### Proposed Fix
Rewrite `bitbucket-pipelines.yml` to reflect the Forge app:
```yaml
# Default: lint + test on every push
# Dev branch: forge deploy -e development
# Master: forge deploy -e staging, then manual step for production
```
- Use `node:20` as the base image.
- Cache `node_modules` properly.
- Run `npm run lint` and `npm test` on every push.
- Add `forge deploy -e development` on pushes to the dev branch.
- Add a manual production deployment step using `forge deploy -e production`.
- Remove all Haskell, Docker, and micros references.

---

## 8. 🌍 Surface Timezone Clearly to Users (UX / Correctness)

**Area:** Frontend / UX  
**Effort:** S (half a day)

### Problem
The app stores and delivers reminders in the user's Jira profile timezone. `siteInfo.js` fetches the site timezone via `GET /rest/api/3/serverInfo`, and `dashboardGadget.jsx` / `viewYourReminders.jsx` both call `view.getContext()` to get the user's timezone. However:
- The user is never told which timezone is being used when they create a reminder in `issueGlance.jsx`.
- If the user's Jira profile timezone differs from their OS timezone, the reminder will fire at an unexpected local time.
- The FAQ says "Reminders use your Jira user profile's timezone setting" but there is no in-app affordance pointing users to where to check or change this.

### Proposed Fix
- Display the resolved timezone as a small hint beneath the date/time picker in `issueGlance.jsx` (e.g. `"Times shown in Australia/Sydney (your Jira profile timezone)"`).
- In `viewYourReminders.jsx` and `dashboardGadget.jsx`, show a footer note with the active timezone.
- Link to the Jira profile timezone settings page from the hint text.
- Update the FAQ to include a direct link to Jira's profile timezone settings.

---

## 9. 📝 Overhaul the README and Keep It in Sync with Forge (Documentation)

**Area:** Documentation  
**Effort:** S (half a day)

### Problem
`README.md` contains several inaccuracies and anachronisms:
- Describes the app as an "Atlassian Connect addon" in the opening paragraph — it has been migrated to Forge.
- Lists Node.js 18+ as a prerequisite — the `.nvmrc` pins Node 20.
- Does not mention the Dhall toolchain (`dhall-to-yaml-ng`) as a prerequisite, yet Dhall compilation is required for every build/deploy step.
- The `"repository"` field in `package.json` points to `github:echo_rm/my-reminders`, which appears to be a placeholder/incorrect value.
- There is no mention of the `forge tunnel` command for live development, which is the primary development loop.
- Does not document the webtrigger and how to use it for local testing.

### Proposed Fix
- Rewrite the opening paragraph to accurately describe the Forge platform.
- Update prerequisites to Node 20 and add `dhall-to-yaml-ng`.
- Add a "Local Development" section documenting `forge tunnel` and the webtrigger manual trigger commands (`npm run trigger:dev`, `npm run trigger:local`).
- Fix the `repository` field in `package.json` to point to the actual repository URL.
- Add a short architecture diagram (text-based Mermaid) showing the scheduler → queue → consumer flow.

---

## 10. ♻️ Deduplicate Table Rendering Logic Across UI Surfaces (Code Quality / Maintainability)

**Area:** Frontend / Code Quality  
**Effort:** M (1 day)

### Problem
The reminder table rendering logic is **copy-pasted** between `viewYourReminders.jsx` and `dashboardGadget.jsx`. Both files independently:
- Define identical `head` cell configurations (`date`, `issue`, `message`).
- Map `allReminders` to row objects with identical `moment.unix(reminder.date)` / `toDateOutput()` / `Link` patterns.
- Independently fetch `siteInfo` and `userTimezone` with identical `useEffectAsync` hooks.

This means any change to the table (e.g. adding an "issue summary" column, changing date formatting) must be made in two places, creating a maintenance burden and a risk of drift between surfaces.

### Proposed Fix
- Extract a shared `<RemindersTable>` component to `src/frontend/remindersTable.jsx` that accepts `reminders`, `siteInfo`, `userTimezone`, and an optional `onDelete` prop.
- Extract a `useRemindersData()` custom hook that encapsulates the three `useEffectAsync` calls (reminders, siteInfo, timezone) shared by both components.
- Replace the duplicated logic in both `viewYourReminders.jsx` and `dashboardGadget.jsx` with the new shared component and hook.
- This also makes it trivial to add the same table to any future UI surface (e.g. a Confluence macro or a Jira project page gadget).

---

## Summary Table

| # | Improvement | Area | Effort | Impact |
|---|---|---|---|---|
| 1 | Fix `deleteAllReminders` bug | Correctness | XS | 🔴 High |
| 2 | Retry logic for failed sends | Reliability | M | 🔴 High |
| 3 | Message length limit | Security / Data Integrity | S | 🟠 Medium |
| 4 | Structured logging & observability | Observability | M | 🟠 Medium |
| 5 | Fix randomised quick-pick times & past-date bugs | UX / Design | S–M | 🟠 Medium |
| 6 | Automated test suite | Quality / DX | L | 🔴 High |
| 7 | Update CI/CD pipeline for Forge | DevOps | M | 🟠 Medium |
| 8 | Surface timezone clearly to users | UX / Correctness | S | 🟡 Low–Medium |
| 9 | Overhaul README documentation | Documentation | S | 🟡 Low–Medium |
| 10 | Deduplicate table rendering logic | Code Quality | M | 🟡 Low–Medium |

**Recommended starting order:** #1 → #5 → #2 → #3 → #6 → #10 → #4 → #7 → #8 → #9
