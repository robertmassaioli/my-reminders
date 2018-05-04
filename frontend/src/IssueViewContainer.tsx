import * as React from 'react';
import { RouteComponentProps } from 'react-router';
import { PageContext } from './page-context';
import EmptyState from '@atlaskit/empty-state';
import { IssueView } from './IssueView';
import { ReminderView, DialogEventData } from './Data';
import * as moment from 'moment';
import 'whatwg-fetch';
import { requestUserDetails, UserDetails, requestIssueDetails } from './HostRequest';
import { ReminderResponse, ReminderRequest } from './reminders-client';
import { createIndividualReminderApi, createIssueRemindersApi } from './api';

type IssueViewContainerProps = {
    pageContext: PageContext;
};

type LoadedDetails = {
    reminders: ReminderView[];
    issue: {
        key: string;
        id: number;
        summary: string;
    },
    user: {
        key: string;
        emailAddress?: string;
    }
};

type IssueViewContainerState = {
    state: 'loading' | 'loaded';
    loadedDetails?: LoadedDetails | 'reminders-failed-to-load';
};

function fetchRemindersForIssue(issueId: number, pageContext: PageContext): Promise<ReminderResponse[]> {
    return createIssueRemindersApi(pageContext).getRemindersForIssue(issueId);
}

function createReminder(data: ReminderRequest, pc: PageContext): Promise<void> {
    return createIndividualReminderApi(pc).addReminder(data).then(() => undefined);
}

function randomIntegerInRange(start: number, end: number): number {
    const smallest = Math.min(start, end);
    const largest = Math.max(start, end);
    const difference = largest - smallest;
    return smallest + Math.floor(Math.random() * difference);
}

function setToMorningHour(date: moment.Moment): moment.Moment {
    date.hours(randomIntegerInRange(6, 8)).minutes(randomIntegerInRange(0, 60));
    return date;
}

export class IssueViewContainer
    extends React.PureComponent<RouteComponentProps<void> & IssueViewContainerProps, IssueViewContainerState> {

    private static calculateReminderViews(userDetails: UserDetails, reminders: ReminderResponse[]): ReminderView[] {
        return reminders.map(r => {
            const expiry = moment(r.date).tz(userDetails.timeZone);
            return {
                id: r.reminderId,
                message: r.message,
                expiresAt: expiry
            };
        });
    }
     
    componentWillMount() {
        this.setState({
            state: 'loading'
        });

        // Handle dialog close events
        AP.events.on('dialog.close', data => {
            const dialogData = data as DialogEventData;
            this.onDialogClosed(dialogData);
        });
    }

    componentWillUnmount() {
        // Removing the dialog close event handler
        AP.events.offAll('dialog.close');
    }

    componentDidMount() {
        this.refreshRemindersList();
    }

    componentDidUpdate() {
        AP.resize();
    }

    render() {
        const ld = this.state.loadedDetails;
        if (typeof ld !== 'undefined' && ld === 'reminders-failed-to-load') {
            return (
                <EmptyState 
                    header="Could not load reminders"
                    description="We could not load the reminders for this issue. Please 
                            try to reload the page and if problems persist then get help."
                    size="narrow"                            
                    secondaryAction={<a href="/redirect/raise-issue">Get help</a>}
                />
            );
        }

        return (
            <IssueView 
                reminders={ld ? ld.reminders : undefined} 
                onAddReminder={() => this.onOpenAdvanced()} 
                onTomorrow={() => this.createReminder(setToMorningHour(moment().add(1, 'days')))} 
                onInAWeek={() => this.createReminder(setToMorningHour(moment().add(1, 'week')))}  
                onInAMonth={() => this.createReminder(setToMorningHour(moment().add(1, 'month')))} 
                onReminderDeleted={id => this.onDeleteReminder(id)}
            />
        );
    }

    private onOpenAdvanced(): void {
        AP.dialog.create({
            key: 'create-reminder-dialog',
            size: 'small'
        });
    }

    private onDialogClosed(data: DialogEventData): void {
        if (data.type === 'create') {
            const message = data.message && data.message.length > 0 ? data.message : undefined;
            const time = moment(data.isoDateTime, moment.ISO_8601);
            this.createReminder(time, message);
        }
    }

    private onDeleteReminder(reminderId: number) {
        createIndividualReminderApi(this.props.pageContext).deleteReminder(reminderId).then(() => {
            this.setState(s => {
                if (s.loadedDetails && s.loadedDetails !== 'reminders-failed-to-load') {
                    return {
                        state: s.state,
                        loadedDetails: {
                            ...s.loadedDetails,
                            reminders: s.loadedDetails.reminders.filter(r => r.id !== reminderId)
                        }
                    };
                } else {
                    return s;
                }
            });
        }).catch(() => {
            AP.flag.create({
                title: 'Could not delete reminder', 
                body: `Please try again and refresh the page if the problem persists.`,
                type: 'error',
                close: 'auto'
            });
        });
    }

    private refreshRemindersList() {
        const issue = this.props.pageContext.issue;
        const user = this.props.pageContext.user;

        if (issue && user) {
            const userRequest = requestUserDetails(user.key);
            const issueRequest = requestIssueDetails(issue.key);
            const remindersRequest = fetchRemindersForIssue(issue.id, this.props.pageContext);

            Promise.all([userRequest, issueRequest, remindersRequest])
            .then(([userDetails, issueDetails, reminders]) => {
                this.setState({
                    state: 'loaded',
                    loadedDetails: {
                        reminders: IssueViewContainer.calculateReminderViews(userDetails, reminders),
                        issue: {
                            id: issue.id,
                            key: issue.key,
                            summary: issueDetails.fields.summary
                        }, 
                        user: {
                            key: user.key,
                            emailAddress: userDetails.emailAddress
                        }
                    }
                });
            }).catch(() => {
                this.setState({
                    loadedDetails: 'reminders-failed-to-load'
                });
            });
        } else {
            this.setState({
                loadedDetails: 'reminders-failed-to-load'
            });
        }
    }

    private createReminder(forDate: moment.Moment, message?: string) {
        const ld = this.state.loadedDetails;
        if (ld && ld !== 'reminders-failed-to-load' && ld.user.emailAddress) {
            const emailAddress = ld.user.emailAddress;

            const requestData: ReminderRequest = {
                issue: {
                    key: ld.issue.key,
                    id: ld.issue.id,
                    summary: ld.issue.summary
                },
                user: {
                    key: ld.user.key,
                    email: emailAddress
                },
                reminderDate: forDate.toDate()
            };

            if (message) {
                requestData.message = message;
            }

            createReminder(requestData, this.props.pageContext)
            .then(() => {
                AP.flag.create({
                    title: 'Reminder created', 
                    body: `Reminder set for ${emailAddress}`,
                    type: 'success',
                    close: 'auto'
                });
                this.refreshRemindersList();
            })
            .catch(() => {
                AP.flag.create({
                    title: 'Could not create reminder', 
                    body: `Reminder not set for ${emailAddress}`,
                    type: 'error',
                    close: 'auto'
                });
            });
        }
    }
}