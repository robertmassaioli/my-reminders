AJS.$(function() {
   AJS.log("Javascript loaded, lets lock and load!");

   var getMeta = function(name) {
      return AJS.$("meta[name=" + name + "]").attr("content");
   }

   var userKey = getMeta("userKey");

   AJS.log("The user key is: " + userKey);

   // TODO when you begin load all of the reminders
   var init = function() {
      var loadReminders = AJS.$.ajax({
         url: "/rest/user/reminders",
         cache: false,
         type: "GET",
         data: {
            userKey: userKey
         }
      });
   };

   // Give time for the page token to be put in place
   setTimeout(init, 1);
});
