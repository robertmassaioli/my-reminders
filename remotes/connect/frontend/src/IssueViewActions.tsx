import React from 'react';
import Button, { ButtonGroup } from '@atlaskit/button';
import DropList, { Item } from '@atlaskit/droplist';
import ChevronDownIcon from '@atlaskit/icon/glyph/chevron-down';
import WarningIcon from '@atlaskit/icon/glyph/warning';
import Spinner from '@atlaskit/spinner';
import styled from 'styled-components';

export type IssueViewActionsProps = {
    statusIndicator?: 'none' | 'actionInProgress' | 'error';
    onAddReminder(): void;
    onTomorrow(): void;
    onInAWeek(): void;
    onInAMonth(): void;
};

type IssueViewActionsState = {
    optionsOpen: boolean;
};

export class IssueViewActions extends React.PureComponent<IssueViewActionsProps, IssueViewActionsState> {
    private IssueViewActionsContainer = styled.div`
        display: flex;
        justify-content: flex-start;
        align-items: flex-start;
    `;

    private StatusIndicator = styled.div`
        padding-left: 10px;
        margin-top: auto;
        margin-bottom: auto;
    `;

    componentWillMount() {
        this.close();
    }

    render() {
      return (
          <this.IssueViewActionsContainer>
            <ButtonGroup>
                <Button isDisabled={this.actionInProgress()} onClick={this.props.onAddReminder}>Add Reminder</Button>
                <DropList
                    appearance="default"
                    position="right top"
                    isOpen={this.state.optionsOpen}
                    onClick={() => this.toggleOpen()}
                    onOpenChange={() => this.close()}
                    trigger={
                        <Button isSelected={this.state.optionsOpen}>
                            <ChevronDownIcon size="small" label="more"/>
                        </Button>}
                >
                    <Item onActivate={this.props.onTomorrow} isDisabled={this.actionInProgress()}>Tomorrow</Item>
                    <Item onActivate={this.props.onInAWeek} isDisabled={this.actionInProgress()}>In a week</Item>
                    <Item onActivate={this.props.onInAMonth} isDisabled={this.actionInProgress()}>In a month</Item>
                    <Item
                        onActivate={this.props.onAddReminder}
                        isDisabled={this.actionInProgress()}
                    >
                        Select a time...
                    </Item>
                </DropList>
            </ButtonGroup>
            <this.StatusIndicator>
                {this.actionInProgress() && <Spinner />}
                {this.inError() && <WarningIcon label="error"/>}
            </this.StatusIndicator>
          </this.IssueViewActionsContainer>
      );
    }

    private actionInProgress(): boolean {
        return this.props.statusIndicator === 'actionInProgress';
    }

    private inError(): boolean {
        return this.props.statusIndicator === 'error';
    }

    private toggleOpen() {
        this.setState(s => {
            return {
                optionsOpen: !s.optionsOpen
            };
        });
    }

    private close = () => this.setState({ optionsOpen: false });
  }