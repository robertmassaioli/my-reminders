import * as React from 'react';
import { RouteProps } from 'react-router';
import { PageContext } from './page-context';
import EmptyState from '@atlaskit/empty-state';
import { IssueView } from './IssueView';
import { ReminderView } from './Data';
import * as moment from 'moment';
import 'whatwg-fetch';

type IssueViewComponentProps = {
    pageContext: PageContext;
};

type IssueViewComponentState = {
    reminders?: ReminderResponse[] | 'reminders-failed-to-load';
};

type ReminderResponse = {
    ReminderId: number;
    IssueId: number;
    IssueKey: string;
    IssueSummary: string;
    UserKey: string;
    UserEmail: string;
    Message: string;
    Date: string;
};

export class IssueViewComponent 
    extends React.PureComponent<RouteProps & IssueViewComponentProps, IssueViewComponentState> {
     
    componentWillMount() {
        this.setState({});
    }

    componentDidMount() {
        const issue = this.props.pageContext.issue;

        if (issue) {
            fetch(`/rest/reminders?issueId=${issue.id}`, {
                method: 'GET',
                cache: 'no-cache',
                headers: {
                    'X-acpt': this.props.pageContext.acpt
                }
            })
            .then(rsp => rsp.json())
            .then(json => {
                this.setState({
                    reminders: json as ReminderResponse[]
                });
            }).catch(() => {
                this.setState({
                    reminders: 'reminders-failed-to-load'
                });
            });
        } else {
            this.setState({
                reminders: 'reminders-failed-to-load'
            });
        }
    }

    componentDidUpdate() {
        AP.resize();
    }

    render() {
        const reminders = this.state.reminders;
        if (typeof reminders === 'undefined') {
            return <p>Loading issue reminers...</p>;
        } else if (reminders === 'reminders-failed-to-load') {
            return (
                <EmptyState 
                    header="Could not load reminders"
                    description="We could not load the reminders for this issue. Please 
                            try to reload the page and if problems persist then get help."
                    secondaryAction={<a href="/redirect/report-issue">Get help</a>}
                />
            );
        }

        const rvs: ReminderView[] = reminders.map(r => {
            const momentDate = moment(r.Date).tz('Australia/Sydney');
            return {
                id: r.ReminderId,
                message: r.Message,
                expiresAt: momentDate.toDate(),
                timezone: 'Australia/Sydney'
            };
        });

        const noOp = () => {
            // noOp
        };

        return (
            <IssueView reminders={rvs} onAddReminder={noOp} onTomorrow={noOp} onInAWeek={noOp} onInAMonth={noOp} />
        );
    }
}