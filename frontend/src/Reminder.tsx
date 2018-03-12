import * as React from 'react';
import Button from '@atlaskit/button';
import CrossCircleIcon from '@atlaskit/icon/glyph/cross-circle';
import * as moment from 'moment';
import 'moment-timezone-all';
import styled from 'styled-components';
import { ReminderView } from './Data';

export type ReminderProps = {
    reminder: ReminderView;
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
        const expires = moment(r.expiresAt).tz(r.timezone);
        return expires.fromNow().replace(/^in/, 'In');
    }

    render() {
        const r = this.props.reminder;

        return (
            <this.Container>
                <div className="content">
                    <this.Title>{r.message}</this.Title>
                    <this.SubInfo>{Reminder.renderExpiry(r)}</this.SubInfo>
                </div>
                <div className="operations">
                    <Button appearance="subtle"><CrossCircleIcon size="small" label="close" /></Button>
                </div>
            </this.Container>
        );
    }
}