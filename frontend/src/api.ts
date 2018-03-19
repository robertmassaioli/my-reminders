import { PageContext } from './page-context';
import * as rc from './reminders-client';

export function createConfiguration(pc: PageContext): rc.Configuration {
    return {
        apiKey: pc.acpt,
        basePath: '/rest'
    };
}

export function createUserRemindersApi(pc: PageContext): rc.UserRemindersApi {
    return new rc.UserRemindersApi(createConfiguration(pc));
}

export function createIndividualReminderApi(pc: PageContext): rc.IndividualReminderApi {
    return new rc.IndividualReminderApi(createConfiguration(pc));
}

export function createIssueRemindersApi(pc: PageContext): rc.IssueRemindersApi {
    return new rc.IssueRemindersApi(createConfiguration(pc));
}