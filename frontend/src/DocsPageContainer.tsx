import * as React from 'react';
import { RouteComponentProps } from 'react-router';
import * as ReactMarkdown from 'react-markdown';

type Params = {
    page: string;
};

type DocsPageContainerState = {
    mdContent: string;
};

export class DocsPageContainer extends React.PureComponent<RouteComponentProps<Params>, DocsPageContainerState> {
    private static pages = {
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
            <div>
                <ReactMarkdown source={this.state.mdContent} />
            </div>
        );
    }

    private setNotFound() {
        this.setState({
            mdContent: 'Failed to load page content.'
        });
    }
}