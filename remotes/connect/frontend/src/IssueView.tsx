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
                    <SectionMessage appearance='warning' title='June 2024: This screen will be retired'>
                        In June 2024, this screen will be retired. Please upgrade to the latest version of the App before then
                        to access the latest screens. Reminders that you can see on this screen that have not expired by June 2024
                        will be sent early. You will be asked to re-create them using the new screens that this app provides.
                        You will also be unable to create any new reminders on this screen from
                        We apologise for the inconvenience as we continue to improve My Reminders.
                    </SectionMessage>
                </IssueView.MessageContainer>
                <IssueView.MessageContainer>
                    <SectionMessage appearance='warning' title='March 2024: No new reminders via this screen'>
                        Please upgrade to the latest version of this App and use the new My Reminders issue screen to create
                        your reminders. In preparation for the retirement of this current screen, you will be blocked from creating
                        any reminders with an expiry date on, or after, March 2024.
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