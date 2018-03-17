import * as React from 'react';
import Button, { ButtonGroup } from '@atlaskit/button';
import { DateTimePicker } from '@atlaskit/datetime-picker';
import { Label } from '@atlaskit/field-base';
import FieldTextArea from '@atlaskit/field-text-area';
import * as moment from 'moment';
import styled from 'styled-components';

export type ReminderCreateDialogProps = {
    onCreate(time: Date, message: string): void;
    onCancel(): void;
};

export class ReminderCreateDialog extends React.PureComponent<ReminderCreateDialogProps> {
    private readonly times = this.generateTimes();

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

    render() {
        const now = moment();
        now.add((10 - now.minutes() % 10), 'minutes');
        now.add(1, 'day');
        const defaultDate = [now.format('YYYY-MM-DD'), now.format('HH:mm')];
        return (
            <this.ReminderCreateContainer>
                <div>
                    <h1>New reminder</h1>
                    <Label label="Due date" />
                    <DateTimePicker onChange={() => this.onDateChange()} times={this.times} defaultValue={defaultDate}/>
                    <FieldTextArea 
                        label="Description" 
                        isSpellCheckEnabled={true}
                        shouldFitContainer={true}
                        enableResize={true}
                        minimumRows={4} 
                        maxLength={2000}
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

    private onDateChange() {
        // TODO
    }

    private generateTimes(): Array<string> {
        const pad = (v: number): string => {
            if (v < 10) {
                return '0' + v.toString();
            }
            return v.toString();
        };

        let t = new Array<string>();
        for (let hour = 0; hour < 24; hour++) {
            for (let minute = 0; minute < 60; minute += 10) {
                t.push(`${pad(hour)}:${pad(minute)}`);
            }
        }
        return t;
    }

    private clickCreate() {
        // TODO pass through the right details
        this.props.onCreate(new Date(), '');
    }
}