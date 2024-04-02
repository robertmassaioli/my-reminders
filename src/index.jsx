// New logic, can just be a .js file
export { issueGlanceHandler } from './resolvers/issueGlance';
export { sendRemindersHandler } from './resolvers/sendReminders';
export { yourRemindersHandler } from './resolvers/yourReminders';
export { dashboardGadgetHandler } from './resolvers/dashboardGadget';
//TODO Old file for UI Kit 1

import { WhereConditions, storage } from "@forge/api";
import moment from 'moment';
import { Queue } from '@forge/events';

// Why does my webtrigger return HTTP 424: what dependency am I missing?
export async function scheduleExpiryJobs() {
  const expiredReminders = new Array();

  // Get all of the expired reminders
  const currentDate = moment().endOf('hour');
  let endOfCurrentHour = currentDate.clone();
  let daysBackSearch = 5;
  for (let daysBack = 0; daysBack < daysBackSearch; daysBack++) {
    let expiredRemindersResult = await storage
      .entity("reminder")
      .query()
      .index("expired-reminders", {
        partition: [endOfCurrentHour.format('YYYY-MM-DD')]
      })
      .where(WhereConditions.isLessThan(currentDate.unix()))
      .getMany();

    expiredReminders.push(...expiredRemindersResult.results);

    // Get all of the expired reminders for this workspace
    while (expiredRemindersResult.nextCursor) {
      expiredRemindersResult = await storage
        .entity("reminder")
        .query()
        .index("expired-reminders", {
          partition: [endOfCurrentHour.format('YYYY-MM-DD')]
        })
        .where(WhereConditions.isLessThan(currentDate.unix()))
        .cursor(expiredRemindersResult.nextCursor)
        .getMany();

        expiredReminders.push(...expiredRemindersResult.results);
    }

    endOfCurrentHour = endOfCurrentHour.subtract(1, 'day');
  }

  // Create new events for all of the expired reminders
  const queue = new Queue({ key: 'expiredReminders' });

  // Note: If we fail to schedule the job this time round then it will still be around
  // For next time.
  if (expiredReminders.length > 0) {
    const queuedJobs = await Promise.allSettled(expiredReminders.map(async expiredReminder => {
      await queue.push(expiredReminder);
    }));

    const rejectedJobs = queuedJobs.filter(job => job.status === 'rejected');
    if (rejectedJobs.length > 0) {
      console.warn(`Tried to schedule ${expiredReminders.length} reminders to be sent but ${rejectedJobs.length} failed to be queued.`);
    }
  } else {
    console.trace(`No reminders were expired.`);
  }

  return {
    statusCode: 204
  };
}