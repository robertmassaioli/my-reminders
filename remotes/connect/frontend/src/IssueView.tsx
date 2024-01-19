import React from 'react';
import styled from 'styled-components';
import { IssueViewActions, IssueViewActionsProps } from './IssueViewActions';
import { ReminderView } from './Data';
import { Reminder } from './Reminder';
import EmptyState from '@atlaskit/empty-state';
import Spinner from '@atlaskit/spinner';
import SectionMessage from '@atlaskit/section-message';

export type IssueViewProps = {
    reminders: ReminderView[] | undefined;
    timezone: string;
    personalSettingsUrl: string;
    onReminderDeleted(id: number): void;
};

export class IssueView extends React.PureComponent<IssueViewProps & IssueViewActionsProps> {
    private static ReminderContainer = styled.div`
        margin-top: 10px;
    `;

    private static SubInfo = styled.p`
        font-size: small;
        color: #C1C7D0;
        margin-top: 10px;
        margin-bottom: 10px;
    `;

    private static MessageContainer = styled.div`
        padding-bottom: 8px;
    `;

    render() {
        return (
            <div>
                <IssueView.MessageContainer>
                    <SectionMessage appearance='warning' title='In June 2024, this screen will be retired. '>
                        <p>
                            Upgrade to the latest version of the App before June 2024.
                            <ul>
                                <li>Any legacy reminder that has not been sent by June will be sent at that time.</li>
                                <li>You will be unable to create reminders on this panel from March 2024.</li>
                                <li>Once upgraded, you can recreate your reminders using the new panel on the Jira issue (and delete your legacy reminders here).</li>
                            </ul>
                        </p>
                    </SectionMessage>
                </IssueView.MessageContainer>
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
        const { reminders, timezone, personalSettingsUrl } = this.props;
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
                    <>
                        <IssueView.ReminderContainer>{rs}</IssueView.ReminderContainer>
                        <IssueView.SubInfo>Your timezone: <a target="_top" referrerPolicy="noreferrer" href={personalSettingsUrl}>{timezone}</a></IssueView.SubInfo>
                    </>
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