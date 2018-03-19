import * as React from 'react';
import * as ReactMarkdown from 'react-markdown';
import Navigation, { 
    AkGlobalItem, 
    AkNavigationItemGroup, 
    AkNavigationItem, 
    AkContainerTitle 
} from '@atlaskit/navigation';
import Page from '@atlaskit/page';
import Tooltip from '@atlaskit/tooltip';
import MarketplaceIcon from '@atlaskit/icon/glyph/marketplace';
import PageIcon from '@atlaskit/icon/glyph/page';
import IssueRaiseIcon from '@atlaskit/icon/glyph/issue-raise';
import styled from 'styled-components';

import logo from './images/logo.svg';

export type DocsPageProps = {
    mdContent: string;  
    location: string;
};

export class DocsPage extends React.Component<DocsPageProps> {
    private ContentSpacing = styled.div`
        margin: 15px;
    `;

    render() {
        const isCurrentLocation: (loc: string) => boolean = loc => {
            return this.props.location === loc;
        };

        return (
            <Page
                navigation={
                    <Navigation 
                        isCollapsible={false}
                        globalPrimaryIcon={<img src={logo} width={24} height={24} />}
                        globalPrimaryIconAppearance="square"
                        globalPrimaryActions={[
                            <AkGlobalItem key="install" size="medium" onClick={() => this.onClickInstall()}>
                                <Tooltip position="right" content="Install from the Atlassian Marketplace">
                                    <MarketplaceIcon
                                        label="Install"
                                        secondaryColor="inherit"
                                        size="medium"
                                    />
                                </Tooltip>
                            </AkGlobalItem>,
                            <AkGlobalItem key="raiseIssue" size="medium" onClick={() => this.onClickRaiseIssue()}>
                                <Tooltip position="right" content="Raise an issue">
                                    <IssueRaiseIcon
                                        label="Raise an issue"
                                        secondaryColor="inherit"
                                        size="medium"
                                    />
                                </Tooltip>
                            </AkGlobalItem>
                        ]}
                        drawers={[]}
                        containerHeaderComponent={() => (
                            <AkContainerTitle
                                icon={<PageIcon label="Documentation" />}
                                text="Documentation"
                            />
                        )}
                    >
                        <AkNavigationItemGroup>
                            <AkNavigationItem 
                                text="Home"
                                isSelected={isCurrentLocation('/docs/home')}
                                href="/docs/home"
                            />
                            <AkNavigationItem 
                                text="FAQ"
                                isSelected={isCurrentLocation('/docs/faq')}
                                href="/docs/faq"
                            />
                            <AkNavigationItem 
                                text="About"
                                isSelected={isCurrentLocation('/docs/about')}
                                href="/docs/about"
                            />
                        </AkNavigationItemGroup>
                    </Navigation>
                }
            >
                <this.ContentSpacing>
                    <ReactMarkdown source={this.props.mdContent} />
                </this.ContentSpacing>
            </Page>
        );
    }

    private onClickInstall() {
        window.location.href = '/redirect/install';
    }

    private onClickRaiseIssue() {
        window.location.href = '/redirect/raise-issue';
    }
}