import Resolver from '@forge/resolver';
import { getYourReminders } from './reminderPersistence';

const resolver = new Resolver();

function extractViewContext(req) {
  return {
    userAaid: req.context.accountId,
  };
}

resolver.define('getYourReminders', async (req) => {
  return {
    reminders: await getYourReminders(extractViewContext(req))
  }
});

export const dashboardGadgetHandler = resolver.getDefinitions();