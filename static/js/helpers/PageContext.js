define(["../lib/URI", './Meta'], function(URI, Meta) {
    // If you put the context variables in well named locations then this will parse them out for you
    var loadPageContext = function() {
        var queryParams = URI(window.location.href).query(true);

        var pageContext = {};

        // Getting the avaliable context params from here: https://developer.atlassian.com/static/connect/docs/concepts/context-parameters.html

        pageContext.user = {
            id: queryParams["user_id"],
            key: queryParams["user_key"]
        };

        // JIRA Page Context Items
        pageContext.project = {
            id: parseInt(queryParams["project_id"]),
            key: queryParams["project_key"]
        };
        pageContext.issue = {
            id: parseInt(queryParams["issue_id"]),
            key: queryParams["issue_key"]
        };
        pageContext.version = {
            id: parseInt(queryParams["version_id"])
        };
        pageContext.component = {
            id: parseInt(queryParams["component_id"])
        };
        pageContext.profileUser = {
            key: queryParams["profile_user_key"],
            name: queryParams["profile_user_name"]
        };
        pageContext.productBaseUrl = Meta.get('hostBaseUrl');
        pageContext.pluginKey = Meta.get('pluginKey');

        return pageContext;
    };

    return {
        load: loadPageContext
    };
});