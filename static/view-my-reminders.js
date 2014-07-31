AJS.$(function() {
   AJS.log("Javascript loaded, lets lock and load!");
   var templates = {};

   var setupTemplates = function() {
      templates.reminderRow = $('#reminder-row').html();
      Mustache.parse(templates.reminderRow);
   };

   var getMeta = function(name) {
      return AJS.$("meta[name=" + name + "]").attr("content");
   }

   var userKey = getMeta("userKey");
   var baseurl = getMeta("hostBaseUrl");

   AJS.log("The user key is: " + userKey);

   // TODO when you begin load all of the reminders
   var init = function() {
      setupTemplates();

      var loadReminders = AJS.$.ajax({
         url: "/rest/user/reminders",
         cache: false,
         type: "GET",
         data: {
            userKey: userKey
         }
      });

      var userDetails = HostRequest.userDetails(userKey);

      var combinedRequest = AJS.$.when(loadReminders, userDetails);

      combinedRequest.done(function(reminderResponse, userResponse) {
         var reminders = JSON.parse(reminderResponse[0]);
         var user = JSON.parse(userResponse[0]);

         var container = AJS.$("#reminders .container");
         container.empty();
         AJS.$.each(reminders, function(index, reminder) {
            reminder.issueLink = baseurl + "/browse/" + reminder.IssueKey;
            var tzDate = moment(reminder["Date"]).tz(user.timeZone);
            reminder.momentDate = tzDate;
            reminder.fullDate = reminder.momentDate.format('D MMM YYYY hh:mmA');

            container.append(Mustache.render(templates.reminderRow, reminder));
         });
      });
   };

   // Give time for the page token to be put in place
   setTimeout(init, 1);
});
