import * as React from 'react';
import Button, { ButtonGroup } from '@atlaskit/button';
import { RemindersList } from './RemindersList';
import { Reminder } from './Data';
import styled from 'styled-components';

export type AllRemindersViewProps = {
    hostBaseUrl: string;
    reminders?: Reminder[];
    onUpdateEmail: (selectedReminderIds: number[]) => void;
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
            <this.Container>
                <h1>My reminders</h1>
                <p>All of your pending reminders can be viewed here. You can also perform some bulk actions on them.</p>
                <this.Actions>
                    <ButtonGroup>
                        <Button onClick={() => this.onUpdateClicked()}>Update email address</Button>
                        <Button onClick={() => this.onDeleteClicked()}>Delete</Button>
                    </ButtonGroup>
                </this.Actions>
                <RemindersList 
                    hostBaseUrl={this.props.hostBaseUrl}
                    reminders={this.props.reminders} 
                    onChange={ids => this.onSelectionChanged(ids)} 
                />
            </this.Container>
        );
    }

    private onSelectionChanged(selectedReminderIds: number[]) {
        this.setState({
            selectedReminderIds: selectedReminderIds
        });
    }

    private onUpdateClicked() {
        this.props.onUpdateEmail(this.state.selectedReminderIds);
    }

    private onDeleteClicked() {
        this.props.onDelete(this.state.selectedReminderIds);
    }
}