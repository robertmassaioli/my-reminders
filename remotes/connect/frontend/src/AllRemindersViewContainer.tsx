import React from 'react';
import { RouteComponentProps } from 'react-router';
import { PageContext } from './page-context';
import { AllRemindersView } from './AllRemindersView';
import { ReminderResponse } from './reminders-client';
import { createUserRemindersApi } from './api';
import { Reminder } from './Data';

export type ARVProps = {
    pageContext: PageContext;
};

type Props = ARVProps & RouteComponentProps<void>;

type ARVState = {
    reminders?: Reminder[];
};

export class AllRemindersViewContainer extends React.PureComponent<Props, ARVState> {
    private static rrlToReminders(rrl: ReminderResponse[]): Reminder[] {
        return rrl.map(r => {
            return {
                id: r.reminderId,
                key: r.issueKey,
                summary: r.issueSummary,
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
                onDelete={ids => this.onDeleteReminders(ids)}
            />
        );
    }

    private reloadReminderData() {
        createUserRemindersApi(this.props.pageContext).getAllReminders()
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

    private onDeleteReminders(selectedReminderIds: number[]) {
        createUserRemindersApi(this.props.pageContext).deleteReminders({
            reminderIdList: { pids: selectedReminderIds }
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