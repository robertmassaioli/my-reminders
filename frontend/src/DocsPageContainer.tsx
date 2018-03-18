import * as React from 'react';
import { RouteComponentProps } from 'react-router';

type Params = {
    page: string;
};

export class DocsPageContainer extends React.PureComponent<RouteComponentProps<Params>> {
    render() {
        return (
            <div>TODO {this.props.match.params.page}</div>
        );
    }
}