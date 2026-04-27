# My Reminders for Cloud

**[📦 Get it on Atlassian Marketplace](https://marketplace.atlassian.com/apps/1212778)**

My Reminders is a **Jira Cloud** app that lets you set time-based reminders on any Jira issue. When a reminder fires, you receive a Jira notification delivered to your registered email address — no external services, no configuration, everything runs inside Atlassian's infrastructure.

- Set up to **10 reminders per user per issue**, any date/time into the future with an optional custom message
- Quick-pick options for common times (end of day, next Monday, in a month, etc.)
- View and manage all your active reminders from the **Your Reminders** global page or a **dashboard gadget**
- Earns the **"Runs on Atlassian"** badge — all data stays within Atlassian's platform

For user-facing documentation, visit the [project wiki](https://github.com/robertmassaioli/my-reminders/wiki).

---

## Architecture Overview

This app is built on [Atlassian Forge](https://developer.atlassian.com/platform/forge/) — Atlassian's serverless app platform. There is no server to manage.

```
Scheduled trigger (every 5 min)
        │
        ▼
scheduleExpiryJobs()        ← queries KVS for expired reminders
        │
        ▼
Forge consumer queue        ← one event per expired reminder
        │
        ▼
sendExpiredReminder()       ← sends Jira notification, deletes reminder from KVS
```

| Concern | Technology |
|---|---|
| Platform | Atlassian Forge (serverless) |
| Runtime | Node.js 20.x |
| Frontend | React 18 + `@forge/react` (UI Kit 2) |
| Backend | `@forge/resolver` + `@forge/api` |
| Storage | Forge managed KVS (`@forge/kvs`) |
| Async jobs | `@forge/events` consumer queue |
| Date handling | `moment` + `moment-timezone` |
| Config | [Dhall](https://dhall-lang.org/) — compiled to YAML at build time |

---

## Prerequisites

1. **Node.js 20** — pinned in `.nvmrc`; run `nvm use` to activate
2. **Forge CLI** — `npm install -g @forge/cli`
3. **dhall-to-yaml-ng** — required to compile manifests before every deploy; install via the [Dhall releases page](https://github.com/dhall-lang/dhall-haskell/releases) or your package manager
4. **Atlassian account** with developer access to a Jira Cloud site

---

## Quick Start

```bash
# 1. Install dependencies
npm install

# 2. Activate the correct Node version
nvm use

# 3. Log in to Forge
forge login

# 4. Deploy to your local Forge environment
npm run deploy:local

# 5. Install the app on your Jira Cloud site (first time only)
forge install --site <your-site>.atlassian.net
```

---

## Development Workflow

### Live development with tunnel

After the initial deploy, use `forge tunnel` for instant frontend changes without a full redeploy:

```bash
forge tunnel
```

### All deploy commands

| Command | What it does |
|---|---|
| `npm run deploy:local` | Compile Dhall → manifest, deploy to `local` environment |
| `npm run deploy:dev` | Compile Dhall → manifest, deploy to `development` environment |
| `npm run deploy:stg` | Compile Dhall → manifest, deploy to `staging` environment |
| `npm run deploy:prod` | Compile Dhall → manifest, deploy to `production` environment |

### Linting

```bash
npm run lint
```

ESLint is configured to enforce React hooks rules strictly. Run this before pushing.

### Manually triggering reminder sending (dev/local only)

In `local` and `dev` environments, a **webtrigger** is enabled so you can manually fire the reminder scheduler without waiting for the 5-minute cron:

```bash
# Trigger in dev environment
npm run trigger:dev

# Trigger in local environment
npm run trigger:local
```

You can also retrieve the webtrigger URL from within the app UI — it is displayed at the bottom of the issue glance panel when `webtriggerEnabled = True` (set in `config.dev.dhall`).

---

## Configuration

The app uses **Dhall** for all configuration and manifests. Always edit the `.dhall` source files — never edit the compiled `manifest.yml` directly, as it is overwritten on every build.

| File | Purpose |
|---|---|
| `manifest.dhall` | Base manifest (shared across all environments) |
| `manifest.local.dhall` | Local overrides — adds webtrigger module |
| `manifest.dev.dhall` | Dev overrides — adds webtrigger module |
| `manifest.prod.dhall` | Prod overrides — no webtrigger |
| `config.dev.dhall` | Feature flags for dev/local (`webtriggerEnabled = True`) |
| `config.prod.dhall` | Feature flags for prod (`webtriggerEnabled = False`) |

The config files are compiled to `src/config.json` at build time and imported by the frontend to conditionally show/hide dev-only UI.

---

## Storage

All data is stored in **Forge managed KVS** under a single entity type: `reminder`. No external database.

Each reminder stores: `userAaid`, `issueId`, `issueKey`, `issueSummary`, `date`, `day`, `message`, `originalIssueKey`, `originalIssueSummary`, `sendAttempts`.

Five indexes are defined in `manifest.dhall` for efficient querying by expiry date, user, issue, and combinations thereof. Pagination is used whenever fetching full reminder lists.

---

## Project Structure

```
src/
├── index.jsx                        # Entry point; exports all resolver handlers
├── useEffectAsync.js                # Async-safe useEffect wrapper hook
├── frontend/
│   ├── issueGlance.jsx              # Issue sidebar UI (create/view/delete reminders)
│   ├── viewYourReminders.jsx        # "Your Reminders" global page UI
│   ├── dashboardGadget.jsx          # Dashboard gadget UI
│   ├── dateHelpers.js               # Date calculation utilities
│   └── siteInfo.js                  # Fetches Jira site timezone info
└── resolvers/
    ├── issueGlance.js               # Resolver: create/read/delete per issue
    ├── yourReminders.js             # Resolver: list/delete all user reminders
    ├── dashboardGadget.js           # Resolver: reminders for dashboard gadget
    ├── reminderPersistence.js       # Shared KVS helpers (get, delete)
    └── sendReminders.js             # Resolver: send expired reminders, delete after send
```

---

## Learn More

- [Forge Documentation](https://developer.atlassian.com/platform/forge/)
- [Forge UI Kit (UI Kit 2)](https://developer.atlassian.com/platform/forge/ui-kit/)
- [Forge Storage API](https://developer.atlassian.com/platform/forge/storage/)
- [Dhall language](https://dhall-lang.org/)
- [Bug reports & feature requests](https://github.com/robertmassaioli/my-reminders/issues)
