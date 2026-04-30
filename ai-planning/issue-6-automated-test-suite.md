# Issue 6 — Add an Automated Test Suite

**Branch:** `improvement/6-automated-test-suite`  
**Area:** Developer Experience / Quality  
**Effort:** L (3–5 days)  
**Status:** ✅ Core implementation complete — all 29 tests passing

---

## Problem Statement

There were zero automated tests in the repository. Logic bugs like the `deleteAllReminders` undefined-variable bug and non-deterministic quick-pick date helpers could only be caught by manual testing or code review. The CI pipeline ran linting only, with no unit or integration tests, and no coverage enforcement.

---

## What Has Been Done

### 1. Tooling & Configuration

- **Jest** (`^30.3.0`) added as a dev dependency along with **Babel** (`@babel/core`, `@babel/preset-env`, `babel-jest`) to transpile ES module syntax for the Node test environment.
- **`babel.config.json`** created, targeting Node 20 (should be updated to Node 24 to match the upgraded runtime — see Outstanding Work below).
- **`"test": "jest"`** script added to `package.json`.
- **Jest config** added inline in `package.json`:
  - `testEnvironment: "node"`
  - **70% line coverage threshold** enforced globally via `coverageThreshold`
  - `collectCoverageFrom` scoped to the three testable modules:
    - `src/frontend/dateHelpers.js`
    - `src/resolvers/reminderPersistence.js`
    - `src/resolvers/sendReminders.js`

### 2. Test Files

Three test files created under `src/__tests__/`:

#### `dateHelpers.test.js`
Tests all pure date utility functions in `src/frontend/dateHelpers.js`:
- `toDateOutput()` — format correctness (AM/PM), non-mutation of input
- All 10 quick-pick helpers — verified to always return future moments
- Specific structural checks: correct hour, correct day-of-week, correct day-of-month, correct quarter months
- All helpers verified to return zero seconds (deterministic, no random component)

#### `reminderPersistence.test.js`
Tests `src/resolvers/reminderPersistence.js` with `@forge/kvs` fully mocked:
- `deleteReminder` — deletes when the calling user is the owner
- `deleteReminder` — refuses and logs a SECURITY ALERT when a different user attempts deletion
- `deleteReminder` — logs an error and does not delete when the reminder no longer exists
- `getYourReminders` — returns all results from a single page
- `getYourReminders` — correctly paginates through multiple pages (cursor-based)

#### `sendReminders.test.js`
Tests the HTML/text email generation helpers exported from `src/resolvers/sendReminders.js`, with `@forge/resolver`, `@forge/api`, and `@forge/kvs` all mocked (since they are unavailable outside the Forge runtime):
- `tag()` — self-closing, single child, multiple children concatenated
- `tagWithAttributes()` — falls back to `tag()` with no attributes, renders single and multiple attributes, self-closing with attributes
- Composition tests verifying `tag` + `tagWithAttributes` produce correct nested HTML structures
- Contract test for `generateTextBody`'s use of self-closing `<br />` syntax

### 3. Current Test Results

```
Test Suites: 3 passed, 3 total
Tests:       29 passed, 29 total
Snapshots:   0 total
Time:        ~0.5s
```

---

## Outstanding Work

### 🔴 Must Do

1. **Update `babel.config.json` target from Node 20 to Node 24**  
   The Forge runtime has been upgraded to `nodejs24.x`. The Babel target should be kept in sync to ensure transpilation correctly reflects the runtime environment.  
   File: `my-reminders/babel.config.json`  
   Change: `"node": "20"` → `"node": "24"`

2. **Add `npm test` to the CI pipeline**  
   `bitbucket-pipelines.yml` currently only runs linting. The test step should be added to the default (every push) pipeline step so tests are enforced on every PR.  
   Note: This overlaps with Improvement #7 (CI/CD pipeline overhaul) — coordinate with that work.

3. **Run tests with coverage in CI**  
   Use `jest --coverage` in the pipeline to enforce the 70% line coverage threshold automatically on every build. A failing coverage threshold should fail the pipeline step.

### 🟡 Should Do

4. **Expand `sendReminders.js` test coverage to `generateHtmlBody` / `generateTextBody` directly**  
   Currently these functions are not exported, so they are tested only indirectly via their primitive helpers (`tag`, `tagWithAttributes`). Consider exporting them (or extracting them to a separate module) to allow direct testing of the moved-issue email scenario (where `originalIssueKey` and `originalIssueSummary` are present).

5. **Add tests for `issueGlance.js` resolver — specifically the `createReminder` limit enforcement**  
   The 10-reminder-per-issue cap is a critical product constraint enforced in `src/resolvers/issueGlance.js`. It currently has no test coverage. A test should verify that the 11th reminder attempt is rejected with the correct error.

6. ~~**Add tests for `yourReminders.js` — specifically the `deleteAllReminders` bug**~~  
   ~~The known bug (referencing undefined `req`) should be captured as a failing test, so it is visible and fixed explicitly rather than silently left broken.~~  
   **N/A** — `deleteAllReminders` was already removed as part of Improvement #1. No action needed.

### 🟢 Nice to Have

7. **Increase coverage threshold from 70% to 80%+**  
   Once items 4–6 above are implemented, the additional coverage should comfortably push past 80%. Raise the threshold to lock it in.

8. **Add a pre-push Git hook to run tests**  
   The repo already uses Husky for commit message linting. A `pre-push` hook running `npm test` would catch regressions before they reach the remote, complementing the CI check.

---

## File Inventory

| File | Status | Notes |
|---|---|---|
| `package.json` | ✅ Updated | Jest + Babel deps added; `test` script added; Jest config inline |
| `babel.config.json` | ⚠️ Needs update | Created; Node target should be bumped from 20 → 24 |
| `src/__tests__/dateHelpers.test.js` | ✅ Complete | 16 tests; all passing |
| `src/__tests__/reminderPersistence.test.js` | ✅ Complete | 5 tests; all passing |
| `src/__tests__/sendReminders.test.js` | ✅ Complete | 8 tests; all passing |
| `bitbucket-pipelines.yml` | ❌ Not updated | `npm test` not yet added to pipeline |

---

## How to Run Tests Locally

```bash
# Run all tests
npm test

# Run with coverage report
npm test -- --coverage

# Run a single test file
npm test -- src/__tests__/dateHelpers.test.js

# Run in watch mode during development
npm test -- --watch
```
