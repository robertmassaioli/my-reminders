import React from 'react';
import Button, { ButtonGroup } from '@atlaskit/button';
import { RemindersList } from './RemindersList';
import { Reminder } from './Data';
import styled from 'styled-components';
import SectionMessage from '@atlaskit/section-message';

export type AllRemindersViewProps = {
    hostBaseUrl: string;
    reminders?: Reminder[];
    onDelete: (selectedReminderIds: number[]) => void;
};

type AllRemindersViewState = {
    selectedReminderIds: number[];
};

export class AllRemindersView extends React.PureComponent<AllRemindersViewProps, AllRemindersViewState> {
    private Container = styled.section`
        margin-top: 10px;
        margin-bottom: 10px;
        margin-left: 25px;
        margin-right: 25px;
    `;

    private Actions = styled.div`
        margin-top: 15px;
        margin-bottom: 15px;
    `;

    componentWillMount() {
        this.setState({
            selectedReminderIds: []
        });
    }

    render() {
        return (
            <>
                <SectionMessage appearance='warning' title='June 2024: This screen will be retired'>
                    In June 2024, this screen will be retired. Please upgrade to the latest version of the App before then
                    to access the latest screens. Reminders that you can see on this screen that have not expired by June 2024
                    will be sent early. You will be asked to re-create them using the new screens that this app provides.
                    We apologise for the inconvenience as we continue to improve My Reminders.
                </SectionMessage>
                <this.Container>
                    <h1>My reminders (legacy)</h1>
                    <p>All of your pending reminders can be viewed here. You can also perform some bulk actions on them.</p>
                    <this.Actions>
                        <ButtonGroup>
                            <Button onClick={() => this.onDeleteClicked()}>Delete</Button>
                        </ButtonGroup>
                    </this.Actions>
                    <RemindersList
                        hostBaseUrl={this.props.hostBaseUrl}
                        reminders={this.props.reminders}
                        onChange={ids => this.onSelectionChanged(ids)}
                    />
                </this.Container>
            </>
        );
    }

    private onSelectionChanged(selectedReminderIds: number[]) {
        this.setState({
            selectedReminderIds: selectedReminderIds
        });
    }

    private onDeleteClicked() {
        this.props.onDelete(this.state.selectedReminderIds);
    }
}