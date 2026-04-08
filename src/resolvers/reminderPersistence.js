import kvs, { Sort } from "@forge/kvs";
import { isPresent } from 'ts-is-present';

export async function deleteReminder(reminderKey, viewUserAaid) {
  const reminder = await kvs.entity('reminder').get(reminderKey);
  if (isPresent(reminder)) {
    // Only if the current user owns the reminder should it be able to delete it
    if (reminder.userAaid === viewUserAaid) {
      await kvs.entity('reminder').delete(reminderKey);
    } else {
      console.error(`SECURITY ALERT: User ${viewUserAaid} tried to delete reminder for ${reminder.userAaid} with key ${reminderKey}`);
    }
  } else {
    console.error(`Tried to delete a reminder that no longer exists: ${reminderKey} as user ${viewUserAaid}`)
  }
}

export async function getYourReminders(viewContext) {
  const allReminders = new Array();

  let result = await kvs
    .entity('reminder')
    .query()
    .index("by-aaid", {
      partition: [viewContext.userAaid]
    })
    .sort(Sort.ASC)
    .getMany();

  allReminders.push(...result.results);

  while (result.nextCursor) {
    result = await kvs
      .entity('reminder')
      .query()
      .index("by-aaid", {
        partition: [viewContext.userAaid]
      })
      .sort(Sort.ASC)
      .cursor(result.nextCursor)
      .getMany();

    allReminders.push(...result.results);
  }

  return allReminders;
}