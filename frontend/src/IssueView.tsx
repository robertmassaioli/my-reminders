import * as React from 'react';
import styled from 'styled-components';
import { IssueViewActions, IssueViewActionsProps } from './IssueViewActions';
import { ReminderView } from './Data';
import { Reminder } from './Reminder';
import EmptyState from '@atlaskit/empty-state';

export type IssueViewProps = {
    reminders: ReminderView[];
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
        if (this.props.reminders.length > 0) {
            const rs = this.props.reminders.map(r => {
                return (
                    <Reminder key={r.id} reminder={r} />
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