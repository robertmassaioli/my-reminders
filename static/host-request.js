var HostRequest = (function() {
   "use strict";

   var requestUserDetails = function(userkey) {
      return AJS.$.Deferred(function() {
         var self = this;
         AP.request({ 
            url: "/rest/api/latest/user", 
            type: "GET",
            cache: true,   // This does not work thanks to https://ecosystem.atlassian.net/browse/AC-1253
            data: { 
               key: userkey
            },
            success: self.resolve,
            fail: self.reject
         });
      });
   };

   var requestIssueDetalis = function(issueKey) {
      return AJS.$.Deferred(function() {
         var self = this;
         AP.request({
            url: "/rest/api/latest/issue/" + issueKey,
            type: "GET",
            cache: true,  // This does not work thanks to https://ecosystem.atlassian.net/browse/AC-1253
            success: self.resolve,
            fail: self.reject
         });
      });
   };
   
   return {
      userDetails: requestUserDetails,
      issueDetails: requestIssueDetalis
   };
}());
