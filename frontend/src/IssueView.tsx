import * as React from 'react';
import styled from 'styled-components';
import { IssueViewActions, IssueViewActionsProps } from './IssueViewActions';
import { ReminderView } from './Data';
import { Reminder } from './Reminder';
import EmptyState from '@atlaskit/empty-state';
import Spinner from '@atlaskit/spinner';

export type IssueViewProps = {
    reminders: ReminderView[] | undefined;
    onReminderDeleted(id: number): void;
};

export class IssueView extends React.PureComponent<IssueViewProps & IssueViewActionsProps> {
    private static ReminderContainer = styled.div`
        margin-top: 10px;
    `;

    render() {
        return (
            <div>
                <IssueViewActions 
                    statusIndicator={this.props.statusIndicator}
                    onAddReminder={this.props.onAddReminder}
                    onTomorrow={this.props.onTomorrow}
                    onInAWeek={this.props.onInAWeek}
                    onInAMonth={this.props.onInAMonth}
                />
                {this.ReminderView()}
            </div>
        );
    }

    private ReminderView(): JSX.Element {
        const reminders = this.props.reminders;
        if (!reminders) {
            return (
                <Spinner size="small" />
            );
        } else {
            if (reminders.length > 0) {
                const rs = reminders.sort((a, b) => a.expiresAt.isBefore(b.expiresAt) ? -1 : 1)
                .map(r => {
                    return (
                        <Reminder key={r.id} reminder={r} onDelete={() => this.props.onReminderDeleted(r.id)} />
                    );
                });
                return (
                    <IssueView.ReminderContainer>{rs}</IssueView.ReminderContainer>
                );
            } else {
                return (
                    <EmptyState 
                        header="No reminders"
                        description="There are no pending reminders for this issue. Create some!"
                        size="narrow"
                    />
                );
            }
        }
    }
}