import Resolver from '@forge/resolver';
import api, { SortOrder, route, storage } from "@forge/api";
import { deleteReminder } from './reminderPersistence';

const resolver = new Resolver();

function extractViewContext(req) {
  return {
    userAaid: req.context.accountId,
  };
}

async function getYourReminders(viewContext) {
  const allReminders = new Array();

  let result = await storage
    .entity('reminder')
    .query()
    .index("by-aaid", {
      partition: [viewContext.userAaid]
    })
    .sort(SortOrder.ASC)
    .getMany();

  allReminders.push(...result.results);

  while (result.nextCursor) {
    result = await storage
      .entity('reminder')
      .query()
      .index("by-aaid", {
        partition: [viewContext.userAaid]
      })
      .sort(SortOrder.ASC)
      .cursor(result.nextCursor)
      .getMany();

    allReminders.push(...result.results);
  }

  return allReminders;
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