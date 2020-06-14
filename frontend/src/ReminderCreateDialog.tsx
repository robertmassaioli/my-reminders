import React from 'react';
import Button, { ButtonGroup } from '@atlaskit/button';
import { DatePicker } from '@atlaskit/datetime-picker';
import TimePicker from 'rc-time-picker';
import { Label } from '@atlaskit/field-base';
import FieldTextArea from '@atlaskit/field-text-area';
import moment from 'moment';
import styled from 'styled-components';

import 'rc-time-picker/assets/index.css';

export type ReminderCreateDialogProps = {
    onCreate(isoDateTime: string, message?: string): void;
    onCancel(): void;
};

type ReminderCreateDialogState = {
    selectedTime: moment.Moment;
    message?: string;
};

export class ReminderCreateDialog extends React.PureComponent<ReminderCreateDialogProps, ReminderCreateDialogState> {
    // tslint:disable:max-line-length
    private ReminderCreateContainer = styled.div`
        padding-left: 30px;
        padding-right: 30px;
        padding-top: 15px;
        padding-bottom: 15px;

        background-color: rgb(255, 255, 255);
        box-shadow: rgba(9, 30, 66, 0.08) 0px 0px 0px 1px, rgba(9, 30, 66, 0.08) 0px 2px 1px, rgba(9, 30, 66, 0.31) 0px 0px 20px -6px;

        display: flex;
        flex-direction: column;
        justify-content: space-between;
    `;
    // tslint:enable:max-line-length

    private ReminderCreateActions = styled.div`
        margin-top: 20px;
        display: flex;
        justify-content: flex-end;
    `;

    private Subtle = styled.p`
        font-size: small;
        color: #C1C7D0;
    `;

    componentWillMount() {
        const now = moment();
        now.add((10 - now.minutes() % 10), 'minutes');
        now.add(1, 'day');

        this.setState({
            selectedTime: now
        });
    }

    render() {
        return (
            <this.ReminderCreateContainer>
                <div>
                    <h1>New reminder</h1>
                    <Label label="Date due" />
                    <DatePicker
                        onChange={(date) => this.onDateChange(date)}
                        defaultValue={this.state.selectedTime.format('YYYY-MM-DD')}
                    />
                    <Label label="Time due" />
                    <TimePicker
                        defaultValue={moment().minute(0)}
                        showSecond={false}
                        use12Hours={true}
                        onChange={val => this.onTimeChange(val)}
                    />
                    <FieldTextArea
                        label="Description"
                        isSpellCheckEnabled={true}
                        shouldFitContainer={true}
                        enableResize={true}
                        minimumRows={4}
                        maxLength={2000}
                        onChange={(e: React.SyntheticEvent<HTMLTextAreaElement>) => this.onMessageChanged(e)}
                    />
                    <this.Subtle>What do you want to be reminded about?</this.Subtle>
                </div>
                <this.ReminderCreateActions>
                    <ButtonGroup>
                        <Button appearance="primary" onClick={() => this.clickCreate()}>Create</Button>
                        <Button onClick={() => this.props.onCancel()}>Cancel</Button>
                    </ButtonGroup>
                </this.ReminderCreateActions>
            </this.ReminderCreateContainer>
        );
    }

    private onDateChange(date: string) {
        this.setState(s => {
            const changedDate = moment(date, 'YYYY-MM-DD');
            changedDate.hour(s.selectedTime.hour());
            changedDate.minute(s.selectedTime.minute());
            return {
                ...s,
                selectedTime: changedDate
            };
        });
    }

    private onTimeChange(time: moment.Moment) {
        this.setState(s => {
            const changedDate = time.clone();
            changedDate.set({
                year: s.selectedTime.year(),
                month: s.selectedTime.month(),
                date: s.selectedTime.date()
            });
            return {
                ...s,
                selectedTime: changedDate
            };
        });
    }

    private onMessageChanged(event: React.SyntheticEvent<HTMLTextAreaElement>) {
        const target = event.target;
        if (target instanceof HTMLTextAreaElement) {
            this.setState(s => {
                return {
                    ...s,
                    message: target.value
                };
            });
        }
    }

    private clickCreate() {
        this.props.onCreate(this.state.selectedTime.toISOString(), this.state.message);
    }
}