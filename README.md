# My Reminders for Cloud

**[📦 Get it on Atlassian Marketplace](https://marketplace.atlassian.com/apps/1212778)**

The My Reminders plugin for JIRA (and eventually the other products too) is an Atlassian Connect addon
that has been designed to complete the notification flow that is missing in Atlassian products. You
can be notified of changes to things that you are interested in but you don't get notifications for
items that do not change.

Every now and then you want to be notified about an important issue that MUST be completed before a
particular date, for example, if another team promised you that a particular issue would be resolved
by a certain date then you would really like to be reminded to check up on that issue by the given
date. That is what the My Reminders plugin for JIRA solves, it will remind you of an issue in a given
amount of time.

For detailed documentation, please visit the [project wiki](https://github.com/robertmassaioli/my-reminders/wiki).

## Development

This app is built using [Atlassian Forge](https://developer.atlassian.com/platform/forge/), a cloud development platform for building Atlassian Cloud apps.

### Prerequisites

1. **Node.js** (version 18 or later) - Check with `node --version`
2. **Forge CLI** - Install globally with `npm install -g @forge/cli`
3. **Atlassian account** with developer access

### Quick Start

1. **Install dependencies:**
   ```bash
   npm install
   ```

2. **Log in to Forge:**
   ```bash
   forge login
   ```

3. **Deploy to development environment:**
   ```bash
   npm run deploy:dev
   ```

4. **Install the app on your Atlassian site:**
   ```bash
   forge install --site <your-site>.atlassian.net
   ```

### Development Commands

- `npm run deploy` - Deploy to local development environment
- `npm run deploy:dev` - Deploy to development environment  
- `npm run deploy:stg` - Deploy to staging environment
- `npm run deploy:prod` - Deploy to production environment
- `npm run lint` - Run ESLint on the codebase

### Configuration

The app uses Dhall configuration files for different environments:
- `config.dev.dhall` - Development configuration
- `config.prod.dhall` - Production configuration
- `manifest.*.dhall` - Forge manifest files for different environments

### Storage

This Forge app uses [Atlassian's managed storage](https://developer.atlassian.com/platform/forge/storage/) instead of external databases. Storage entities are defined in the manifest files.

### Learn More

- [Forge Documentation](https://developer.atlassian.com/platform/forge/)
- [Forge React Guide](https://developer.atlassian.com/platform/forge/custom-ui-kit-components/)
- [Forge Storage API](https://developer.atlassian.com/platform/forge/storage/)

 [1]: https://github.com/robertmassaioli/my-reminders