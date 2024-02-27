import Resolver from '@forge/resolver';
import { deleteReminder, getYourReminders } from './reminderPersistence';

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

resolver.define('deleteReminder', async (req) => {
  const { reminderKey } = req.payload;
  const viewContext = extractViewContext(req);
  await deleteReminder(reminderKey, viewContext.userAaid);

  return {
    reminders: await getYourReminders(viewContext)
  }
});

resolver.define('deleteAllReminders', async () => {
  return {
    reminders: await getYourReminders(extractViewContext(req))
  }
});

export const yourRemindersHandler = resolver.getDefinitions();