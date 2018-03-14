import * as React from 'react';
import { RouteProps } from 'react-router';
import { PageContext } from './page-context';
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
    componentDidMount() {
        this.setState({});

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

    render() {
        return (
            <p>{this.state && this.state.reminders ? this.state.reminders.length : 'NA'}</p>
        );
    }
}