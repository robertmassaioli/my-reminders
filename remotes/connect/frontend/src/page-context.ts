import URI from 'urijs';

export type PageContext = {
    user?: {
        accountId: string;
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
    acpt: string;
};

// tslint:disable-next-line: no-any
function numberFromAny(value: any): number | undefined {
    return typeof value === 'string' ? parseInt(value, 10) : undefined;
}

// tslint:disable-next-line: no-any
function es(value: any): string | undefined {
    return typeof value === 'string' ? value : undefined;
}

function fromMeta(name: string): string | undefined {
    const metaTags = Array.from(document.getElementsByTagName('meta'));

    const metaTag = metaTags.find(meta => meta.getAttribute('name') === name);

    if (!metaTag) {
        return undefined;
    }
    const content = metaTag.getAttribute('content');
    return content ? content : undefined;
}

export function loadPageContext(): PageContext | undefined {
    var queryParams = URI(window.location.href).query(true) as { [key: string]: string };

    const pAcpt = fromMeta('acpt');
    const pAaid = fromMeta('userAccountId');
    const pProductBaseUrl = fromMeta('productBaseUrl');
    if (!pProductBaseUrl || !pAcpt || !pAaid) {
        return undefined;
    }

    let pageContext: PageContext = {
        productBaseUrl: pProductBaseUrl,
        acpt: pAcpt,
        user: {
            accountId: pAaid
        }
    };

    /* tslint:disable:no-string-literal */

    const pProjectId = numberFromAny(queryParams['project_id']);
    const pProjectKey = es(queryParams['project_key']);
    const pIssueId = numberFromAny(queryParams['issue_id']);
    const pIssueKey = es(queryParams['issue_key']);
    const pComponentId = numberFromAny(queryParams['component_id']);
    const pProfileUserKey = es(queryParams['profile_user_key']);
    const pProfileUserName = es(queryParams['profile_user_name']);
    /* tslint:enable:no-string-literal */

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