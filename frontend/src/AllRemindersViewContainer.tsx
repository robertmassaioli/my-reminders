import * as React from 'react';
import { RouteProps } from 'react-router';
import { PageContext } from './page-context';
import { AllRemindersView } from './AllRemindersView';
import { ReminderResponseList } from './reminders-client';
import { createDefaultApi } from './api';
import { Reminder } from './Data';

export type ARVProps = {
    pageContext: PageContext;
};

type Props = ARVProps & RouteProps;

type ARVState = {
    reminders?: Reminder[];
};

export class AllRemindersViewContainer extends React.PureComponent<Props, ARVState> {
    private static rrlToReminders(rrl: ReminderResponseList): Reminder[] {
        return rrl.map(r => {
            return {
                id: r.reminderId,
                key: r.issueKey,
                summary: r.issueSummary,
                email: r.userEmail,
                message: r.message,
                expiresAt: r.date
            };
        });
    }

    componentWillMount() {
        this.setState({});
    }

    componentDidMount() {
        this.reloadReminderData();
    }

    componentDidUpdate() {
        AP.resize();
    }

    render() {
        return (
            <AllRemindersView
                hostBaseUrl={this.props.pageContext.productBaseUrl}
                reminders={this.state.reminders}
                onUpdateEmail={ids => this.onRefreshReminders(ids)}
                onDelete={ids => this.onDeleteReminders(ids)}
            />
        );
    }

    private reloadReminderData() {
        createDefaultApi(this.props.pageContext).getAllReminders()
        .then(rrl => {
            this.setState(s => {
                return {
                    ...s,
                    reminders: AllRemindersViewContainer.rrlToReminders(rrl)
                };
            });
        }).catch(() => {
            // todo what is the error state for this component?
        });
    }

    private onRefreshReminders(selectedReminderIds: number[]) {
        createDefaultApi(this.props.pageContext).refreshReminders({
            pids: selectedReminderIds
        }).then(() => {
            this.reloadReminderData();
        }).catch(() => {
            AP.flag.create({
                title: 'Could not update reminders',
                body: 'Please try again and contact support if the problem persists.',
                type: 'error',
                close: 'auto'
            });
        });
    }

    private onDeleteReminders(selectedReminderIds: number[]) {
        createDefaultApi(this.props.pageContext).deleteReminders({
            pids: selectedReminderIds
        }).then(() => {
            this.reloadReminderData();
        }).catch(() => {
            AP.flag.create({
                title: 'Could not delete reminders',
                body: 'Please try again and contact support if the problem persists.',
                type: 'error',
                close: 'auto'
            });
        });
    }
}