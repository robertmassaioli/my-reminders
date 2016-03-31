define(function() {
   "use strict";

   var requestUserDetails = function(userkey) {
      return AJS.$.Deferred(function() {
         var self = this;
         AP.request({ 
            url: "/rest/api/latest/user", 
            type: "GET",
            cache: true,   // This does not work thanks to https://ecosystem.atlassian.net/browse/AC-1253
            dataType: "text",
            data: { 
               key: userkey
            },
            success: self.resolve,
            error: self.reject
         });
      });
   };

   var requestIssueDetails = function(issueKey) {
      return AJS.$.Deferred(function() {
         var self = this;
         AP.request({
            url: "/rest/api/latest/issue/" + issueKey,
            type: "GET",
            cache: true,  // This does not work thanks to https://ecosystem.atlassian.net/browse/AC-1253
            dataType: "text",
            success: self.resolve,
            error: self.reject
         });
      });
   };
   
   return {
      userDetails: requestUserDetails,
      issueDetails: requestIssueDetails
   };
});
