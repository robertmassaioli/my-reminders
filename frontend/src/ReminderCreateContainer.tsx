import * as React from 'react';
import { RouteComponentProps } from 'react-router';
import { PageContext } from './page-context';
import { ReminderCreateDialog } from './ReminderCreateDialog';
import { DialogCreateData, DialogCancelData } from './Data';

type ReminderCreateContainerProps = {
    pageContext: PageContext;
};

type Props = RouteComponentProps<void> & ReminderCreateContainerProps;

export class ReminderCreateContainer extends React.PureComponent<Props> {
    render() {
        return (
            <ReminderCreateDialog 
                onCreate={(date, time, message) => this.onCreate(date, time, message)}
                onCancel={() => this.onCancel()}
            />
        );
    }

    private onCreate(date: string, time: string, message?: string): void {
        const data: DialogCreateData = {
            type: 'create',
            date: date,
            time: time,
            message: message
        };
        AP.dialog.close(data);
    }

    private onCancel(): void {
        const data: DialogCancelData = {
            type: 'cancel'
        };
        AP.dialog.close(data);
    }
}