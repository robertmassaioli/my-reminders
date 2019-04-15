import * as URI from 'urijs';

export interface UserDetails {
    timeZone: string;
    displayName: string;
    name: string;
    key: string;
    accountId: string;
    active: boolean;
}

export function requestUserDetails(): Promise<UserDetails> {
    const url = URI('/rest/api/3/myself');
    return AP.request({
        url: url.toString(),
        type: 'GET'
    }).then(rsp => {
        const parsedBody = JSON.parse(rsp.body);
        return parsedBody as UserDetails;
    });
}

interface IssueDetails {
    fields: IssueFieldDetails;
}

interface IssueFieldDetails {
    summary: string;
}

export function requestIssueDetails(issueKey: string): Promise<IssueDetails> {
    return AP.request({
        url: `/rest/api/2/issue/${issueKey}?fields=summary`,
        type: 'GET'
    }).then(rsp => {
        return JSON.parse(rsp.body) as IssueDetails;
    });
}