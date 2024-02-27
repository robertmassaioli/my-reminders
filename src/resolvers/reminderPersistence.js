import { SortOrder, storage } from "@forge/api";
import { isPresent } from 'ts-is-present';

export async function deleteReminder(reminderKey, viewUserAaid) {
  const reminder = await storage.entity('reminder').get(reminderKey);
  if (isPresent(reminder)) {
    // Only if the current user owns the reminder should it be able to delete it
    if (reminder.userAaid === viewUserAaid) {
      await storage.entity('reminder').delete(reminderKey);
    } else {
      console.log(`SECURITY ALERT: User ${viewUserAaid} tried to delete reminder for ${reminder.userAaid} with key ${reminderKey}`);
    }
  } else {
    console.log(`Tried to delete a reminder that no longer exists: ${reminderKey} as user ${viewUserAaid}`)
  }
}

export async function getYourReminders(viewContext) {
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