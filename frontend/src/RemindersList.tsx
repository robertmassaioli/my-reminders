import * as React from 'react';
import DynamicTable, { DynamicTableHead, DynamicTableRow } from '@atlaskit/dynamic-table';
import EmptyState from '@atlaskit/empty-state';
import * as moment from 'moment';
import { Reminder } from './Data';

export type RemindersListProps = {
    hostBaseUrl: string;
    reminders: Reminder[];
    onChange: (selectedReminderIds: number[]) => void;
};

export type RemindersListState = {
    selectedReminderIds: number[];
};

export class RemindersList extends React.PureComponent<RemindersListProps, RemindersListState> {
    componentWillMount() {
        this.clearSelection();
    }

    render() {
        // date, email, issue, message
        const head: DynamicTableHead = {
            cells: [
                {
                    key: 'operations',
                    content: (
                        <input 
                            type="checkbox" 
                            checked={this.allAreSelected()} 
                            onChange={() => this.selectAllToggle()}
                        />
                    ),
                    isSortable: false
                },
                {
                    key: 'date',
                    content: <span>Date</span>,
                    isSortable: true
                },
                {
                    key: 'email',
                    content: <span>Receiver</span>,
                    isSortable: true
                },
                {
                    key: 'Issue',
                    content: <span>Issue</span>,
                    isSortable: true
                },
                {
                    key: 'Message',
                    content: <span>Message</span>,
                    isSortable: false
                }
            ]
        };

        const emptyState: JSX.Element = (
            <EmptyState 
                header="No reminders" 
                description="You have no reminders that are waiting 
                to be sent. Browse your issues and 
                add reminders to those issues."
            />
        );

        return (
            <DynamicTable 
                head={head}
                rows={this.rowsFromReminders()}
                emptyView={emptyState}
            />
        );
    }

    private clearSelection() {
        this.setState({
            selectedReminderIds: []
        });
    }

    private triggerChange() {
        this.props.onChange(this.state.selectedReminderIds);
    }

    private allAreSelected(): boolean {
        return this.props.reminders.length > 0 && this.state.selectedReminderIds.length === this.props.reminders.length;
    }

    private selectAllToggle() {
        this.setState({
            selectedReminderIds: this.allAreSelected() ? [] : this.props.reminders.map(r => r.id)
        });
        this.triggerChange();
    }

    private reminderChanged(reminderId: number) {
        this.setState(s => {
            const existing = !!s.selectedReminderIds.find(id => id === reminderId);
            const selectedReminderIds = existing 
                ? s.selectedReminderIds.filter(id => id !== reminderId) 
                : [reminderId].concat(s.selectedReminderIds);
            return { selectedReminderIds };
        });
        this.triggerChange();
    }

    private rowsFromReminders(): DynamicTableRow[] {
        return this.props.reminders.map(r => this.rowFromReminder(r));
    }

    private rowFromReminder(reminder: Reminder): DynamicTableRow {
        const reminderDate = moment(reminder.expiresAt).format('D MMM YYYY hh:mmA');
        const hostBaseUrl = this.props.hostBaseUrl.replace(/\/$/, '');
        const isSelected = !!this.state.selectedReminderIds.find(id => id === reminder.id);
        return {
            key: reminder.key,
            cells: [
                {
                    key: 'operation',
                    content: (
                        <input 
                            type="checkbox"
                            checked={isSelected} 
                            onChange={(e) => this.reminderChanged(reminder.id)} 
                        />
                    )
                },
                {
                    key: 'date',
                    content: <span>{reminderDate}</span>
                },
                {
                    key: 'email',
                    content: <span>{reminder.email}</span>
                },
                {
                    key: 'issue',
                    content: <a href={`${hostBaseUrl}/browse/${reminder.key}`}>{reminder.key}: {reminder.summary}</a>
                },
                {
                    key: 'message',
                    content: <span>{reminder.message || ''}</span>
                }
            ]
        };
    }
}