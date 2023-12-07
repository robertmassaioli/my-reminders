import React from 'react';
import Button from '@atlaskit/button';
import CrossCircleIcon from '@atlaskit/icon/glyph/cross-circle';
import 'moment-timezone-all';
import styled from 'styled-components';
import { ReminderView } from './Data';

export type ReminderProps = {
    reminder: ReminderView;
    onDelete: () => void;
};

export class Reminder extends React.PureComponent<ReminderProps> {
    private Container = styled.section`
        display: flex;
        justify-content: space-between;
        align-items: center;
        border-bottom: #C1C7D0 1px solid;
        padding-left: 10px;
        padding-right: 10px;
    `;

    private Title = styled.h1`
        font-size: small;
        margin-top: 10px !important;
        margin-bottom: 5px;
    `;

    private SubInfo = styled.p`
        font-size: small;
        color: #C1C7D0;
        margin-top: 0px;
        margin-bottom: 10px;
    `;

    private static renderExpiry(r: ReminderView): string {
        return r.expiresAt.fromNow().replace(/^in/, 'In');
    }

    private static renderExactTime(r: ReminderView): string {
        return r.expiresAt.format('DD MMM YYYY h:mmA');
    }

    render() {
        const r = this.props.reminder;

        const message = r.message || 'Reminder';
        return (
            <this.Container>
                <div className="content">
                    <this.Title>{message}</this.Title>
                    <this.SubInfo>{Reminder.renderExpiry(r)}  ({Reminder.renderExactTime(r)})</this.SubInfo>
                </div>
                <div className="operations">
                    <Button
                        appearance="subtle"
                        onClick={() => this.props.onDelete && this.props.onDelete()}
                    >
                        <CrossCircleIcon size="small" label="delete" />
                    </Button>
                </div>
            </this.Container>
        );
    }
}