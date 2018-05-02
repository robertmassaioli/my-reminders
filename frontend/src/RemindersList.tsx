import * as React from 'react';
import DynamicTable, { DynamicTableHead, DynamicTableRow } from '@atlaskit/dynamic-table';
import EmptyState from '@atlaskit/empty-state';
import * as moment from 'moment';
import { Reminder } from './Data';
import Spinner from '@atlaskit/spinner';
import styled from 'styled-components';

// Imports for IE11
import 'core-js/fn/array/find';

export type RemindersListProps = {
    hostBaseUrl: string;
    reminders?: Reminder[];
    onChange: (selectedReminderIds: number[]) => void;
};

export type RemindersListState = {
    selectedReminderIds: number[];
};

export class RemindersList extends React.PureComponent<RemindersListProps, RemindersListState> {
    private static Center = styled.div`
        display: flex;
        flex-wrap: wrap;
        text-align: center;
        justify-content: center;
    `;

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

        if (typeof this.props.reminders === 'undefined') {
            return (
                <RemindersList.Center>
                    <Spinner size="large" />
                </RemindersList.Center>
            );
        } else {
            return (
                <DynamicTable 
                    head={head}
                    rows={this.rowsFromReminders(this.props.reminders)}
                    emptyView={emptyState}
                />
            );
        }
    }

    private clearSelection() {
        this.setState({
            selectedReminderIds: []
        });
    }

    private triggerChange(newReminderIds: number[]) {
        this.props.onChange(newReminderIds);
    }

    private allAreSelected(): boolean {
        if (typeof this.props.reminders === 'undefined') {
            return false;
        }
        return this.props.reminders.length > 0 && this.state.selectedReminderIds.length === this.props.reminders.length;
    }

    private selectAllToggle() {
        if (typeof this.props.reminders !== 'undefined') {
            const ids = this.allAreSelected() ? [] : this.props.reminders.map(r => r.id);
            this.setState({
                selectedReminderIds: ids
            });
            this.triggerChange(ids);
        }
    }

    private reminderChanged(reminderId: number) {
        this.setState(s => {
            const existing = !!s.selectedReminderIds.find(id => id === reminderId);
            const selectedReminderIds = existing 
                ? s.selectedReminderIds.filter(id => id !== reminderId) 
                : [reminderId].concat(s.selectedReminderIds);
            this.triggerChange(selectedReminderIds);
            return { selectedReminderIds };
        });
    }

    private rowsFromReminders(reminders: Reminder[]): DynamicTableRow[] {
        return reminders.map(r => this.rowFromReminder(r));
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
                    content: (
                        <a target="_top" href={`${hostBaseUrl}/browse/${reminder.key}`}>
                            {reminder.key}: {reminder.summary}
                        </a>
                    )
                },
                {
                    key: 'message',
                    content: <span>{reminder.message || ''}</span>
                }
            ]
        };
    }
}