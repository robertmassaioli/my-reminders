import * as React from 'react';
import { RouteProps } from 'react-router';
import { PageContext } from './page-context';
import EmptyState from '@atlaskit/empty-state';
import { IssueView } from './IssueView';
import { ReminderView } from './Data';
import * as moment from 'moment';
import 'whatwg-fetch';
import { requestUserDetails, UserDetails, requestIssueDetails } from './HostRequest';
import { DefaultApi, ReminderResponse, ReminderRequest } from './reminders-client';

type IssueViewComponentProps = {
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

type IssueViewComponentState = {
    loadedDetails?: LoadedDetails | 'reminders-failed-to-load';
};

function createDefaultApi(pc: PageContext): DefaultApi {
    return new DefaultApi({
        apiKey: pc.acpt,
        basePath: '/rest'
    });
}

function fetchRemindersForIssue(issueId: number, pageContext: PageContext): Promise<ReminderResponse[]> {
    return createDefaultApi(pageContext).remindersGet(issueId);
}

function createReminder(data: ReminderRequest, pc: PageContext): Promise<void> {
    return createDefaultApi(pc).reminderPut(data).then(() => undefined);
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

export class IssueViewComponent 
    extends React.PureComponent<RouteProps & IssueViewComponentProps, IssueViewComponentState> {

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
        this.setState({});
    }

    componentDidMount() {
        this.refreshRemindersList();
    }

    componentDidUpdate() {
        AP.resize();
    }

    render() {
        const ld = this.state.loadedDetails;
        if (typeof ld === 'undefined') {
            return <p>Loading issue reminers...</p>;
        } else if (ld === 'reminders-failed-to-load') {
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

        const noOp = () => {
            // noOp
        };

        return (
            <IssueView 
                reminders={ld.reminders} 
                onAddReminder={noOp} 
                onTomorrow={() => this.createReminderTomorrow(setToMorningHour(moment().add(1, 'days')))} 
                onInAWeek={() => this.createReminderTomorrow(setToMorningHour(moment().add(1, 'week')))}  
                onInAMonth={() => this.createReminderTomorrow(setToMorningHour(moment().add(1, 'month')))} 
                onReminderDeleted={noOp}
            />
        );
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
                    loadedDetails: {
                        reminders: IssueViewComponent.calculateReminderViews(userDetails, reminders),
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

    private createReminderTomorrow(forDate: moment.Moment) {
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
                // TODO what do we do when this fails? Show the error state for a set amount of time.
            });
        }
    }
}