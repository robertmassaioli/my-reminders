import React from 'react';
import { RouteComponentProps } from 'react-router';
import { DocsPage } from './DocsPage';

type Params = {
    page: string;
};

type DocsPageContainerState = {
    mdContent: string;
};

export class DocsPageContainer extends React.PureComponent<RouteComponentProps<Params>, DocsPageContainerState> {
    private static pages: { [pageId: string]: () => string } = {
        'home': () => require('./docs/home.md'),
        'faq': () => require('./docs/faq.md'),
        'about': () => require('./docs/about.md')
    };

    componentWillMount() {
        this.setState({
            mdContent: 'Loading...'
        });
    }

    componentDidMount() {
        const pageId = this.props.match.params.page;
        if (pageId in DocsPageContainer.pages) {
            const contentUrl = DocsPageContainer.pages[pageId]();

            fetch(contentUrl, {
                method: 'GET'
            }).then(rsp => rsp.text())
            .then(body => {
                this.setState({
                    mdContent: body
                });
            }).catch(() => {
                this.setNotFound();
            });
        } else {
            this.setNotFound();
        }
    }

    render() {
        return (
            <DocsPage mdContent={this.state.mdContent} location={this.props.location.pathname} />
        );
    }

    private setNotFound() {
        this.setState({
            mdContent: 'Failed to load page content.'
        });
    }
}