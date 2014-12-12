define(function() {
    "use strict";

    var fns = {};

    fns.resizeToBottomOf = function(jqueryObject) {
        var width = AJS.$(window).width();
        var height = jqueryObject.position().top + jqueryObject.height();
        AP.resize(width, height);
    };

    return fns;
});