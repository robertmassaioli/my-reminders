import React from 'react';
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
                onCreate={(isoDateTime, message) => this.onCreate(isoDateTime, message)}
                onCancel={() => this.onCancel()}
            />
        );
    }

    private onCreate(isoDateTime: string, message?: string): void {
        const data: DialogCreateData = {
            type: 'create',
            isoDateTime: isoDateTime,
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