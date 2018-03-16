import * as moment from 'moment';

export type Reminder = {
    id: number;
    key: string;
    summary: string;
    email: string;
    message?: string;
    expiresAt: Date;
};

export type ReminderView = {
    id: number;
    message?: string;
    expiresAt: moment.Moment;
};