import * as URI from 'urijs';

export interface UserDetails {
    timeZone: string;
    emailAddress?: string;
    displayName: string;
    name: string;
    key: string;
    accountId: string;
    active: boolean;
}

export function requestUserDetails(userKey: string): Promise<UserDetails> {
    const url = URI('/rest/api/latest/user').addSearch('key', userKey);
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
        url: `/rest/api/latest/issue/${issueKey}`,
        type: 'GET'
    }).then(rsp => {
        return JSON.parse(rsp.body) as IssueDetails;
    });
}