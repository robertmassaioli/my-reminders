define(function() {
    "use strict";

    var methods = {};

    methods.get = function(name) {
        return AJS.$('meta[name=' + name + ']').attr('content');
    };

    return methods;
});