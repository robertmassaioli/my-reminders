import * as URI from 'urijs';

export type PageContext = {
    user?: {
        id: string;
        key: string;
    },
    project?: {
        id: number;
        key: string;
    },
    issue?: {
        id: number;
        key: string;
    },
    component?: {
        id: number;
    },
    profileUser?: {
        key: string;
        name: string;
    },
    productBaseUrl: string;
};

// tslint:disable-next-line: no-any
function numberFromAny(value: any): number | undefined {
    return typeof value === 'string' ? parseInt(value, 10) : undefined;
}

// tslint:disable-next-line: no-any
function es(value: any): string | undefined {
    return typeof value === 'string' ? value : undefined;
}

export function loadPageContext(): PageContext | undefined {
    var queryParams = URI(window.location.href).query(true);

    const metaTags = Array.from(document.getElementsByTagName('meta'));

    const productBaseUrlMeta = metaTags.find(meta => meta.getAttribute('name') === 'productBaseUrl');

    if (!productBaseUrlMeta) {
        return undefined;
    }
    const pProductBaseUrl = productBaseUrlMeta.getAttribute('content');
    if (!pProductBaseUrl) {
        return undefined;
    }

    let pageContext: PageContext = {
        productBaseUrl: pProductBaseUrl
    };

    /* tslint:disable:no-string-literal */
    const pUserId = es(queryParams['user_id']);
    const pUserKey = es(queryParams['user_key']);
    const pProjectId = numberFromAny(queryParams['project_id']);
    const pProjectKey = es(queryParams['project_key']);
    const pIssueId = numberFromAny(queryParams['issue_id']);
    const pIssueKey = es(queryParams['issue_key']);
    const pComponentId = numberFromAny(queryParams['component_id']);
    const pProfileUserKey = es(queryParams['profile_user_key']);
    const pProfileUserName = es(queryParams['profile_user_name']);
    /* tslint:enable:no-string-literal */

    if (pUserId && pUserKey) {
        pageContext.user = {
            id: pUserId,
            key: pUserKey
        };
    }

    if (pProjectId && pProjectKey) {
        pageContext.project = {
            id: pProjectId,
            key: pProjectKey
        };
    }

    if (pIssueId && pIssueKey) {
        pageContext.issue = {
            id: pIssueId,
            key: pIssueKey
        };
    }

    if (pComponentId) {
        pageContext.component = {
            id: pComponentId
        };
    }

    if (pProfileUserKey && pProfileUserName) {
        pageContext.profileUser = {
            key: pProfileUserKey,
            name: pProfileUserName
        };
    }

    return pageContext;
}