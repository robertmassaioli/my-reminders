define(['lib/mustache', 'lib/moment-timezone', 'host/request', 'connect/pagetoken'], function(Mustache, moment, HostRequest) {
   AJS.$(function() {
      AJS.log("Javascript loaded, lets lock and load!");
      var templates = {};

      var setupTemplates = function() {
         templates.reminderRow = $('#reminder-row').html();
         Mustache.parse(templates.reminderRow);
      };

      var getMeta = function(name) {
         return AJS.$("meta[name=" + name + "]").attr("content");
      };

      var userKey = getMeta("userKey");
      var baseurl = getMeta("hostBaseUrl");

      if(userKey === "") {
         AJS.$("#not-logged-in").removeClass("hidden");
         AJS.$("#logged-in-content").addClass("hidden");
      }

      AJS.log("The user key is: " + userKey);

      var remindersContainer = AJS.$("#reminders tbody.container");
      var noRemindersMessage = AJS.$("#no-reminders-message");

      var allReminderSelects = function() {
         return remindersContainer.find(".reminder .select");
      };

      var check = function(element, checked) {
         if(checked) {
            element.attr("checked", "checked");
         } else {
            element.removeAttr("checked");
         }
      };

      var isChecked = function(rawElement) {
         return AJS.$(rawElement).attr("checked") === "checked";
      };

      var setIndeterminateState = function() {
         var allSelects = allReminderSelects();
         var selectedReminders = AJS.$.grep(allSelects, isChecked);

         var masterSelect = AJS.$("#master-selector");
         masterSelect.prop("indeterminate", selectedReminders.length > 0 && selectedReminders.length != allSelects.length);
         check(masterSelect, selectedReminders.length > 0 && selectedReminders.length === allSelects.length);
      };

      var getSelectedReminderIds = function() {
         return AJS.$.map(AJS.$.grep(allReminderSelects(), isChecked), function(reminder) {
            return parseInt(AJS.$(reminder).closest(".reminder").attr("data-reminder-id"));
         });
      };

      var reloadReminders = function() {
         var loadReminders = AJS.$.ajax({
            url: "/rest/user/reminders",
            cache: false,
            type: "GET",
            dataType: "text", 
            data: {
               userKey: userKey
            }
         });

         var userDetails = HostRequest.userDetails(userKey);

         var combinedRequest = AJS.$.when(loadReminders, userDetails);

         combinedRequest.done(function(reminderResponse, userResponse) {
            var reminders = JSON.parse(reminderResponse[0]);
            var user = JSON.parse(userResponse[0]);

            var hasReminders = reminders.length > 0;
            remindersContainer.toggleClass("hidden", !hasReminders);
            noRemindersMessage.toggleClass("hidden", hasReminders);

            remindersContainer.empty();
            if(hasReminders) {
               AJS.$.each(reminders, function (index, reminder)
               {
                  reminder.issueLink = baseurl + "/browse/" + reminder.IssueKey;
                  reminder.momentDate = moment(reminder["Date"]).tz(user.timeZone);;
                  reminder.fullDate = reminder.momentDate.format('D MMM YYYY hh:mmA');

                  remindersContainer.append(Mustache.render(templates.reminderRow, reminder));
               });
            }

            setIndeterminateState();
         });
      };

      // TODO when you begin load all of the reminders
      var init = function() {
         setupTemplates();

         reloadReminders();

         AJS.$("#master-selector").click(function() {
            check(allReminderSelects(), !!AJS.$(this).attr("checked"));
         });

         remindersContainer.on("click", ".reminder .select", function() {
            setIndeterminateState();
            console.log(getSelectedReminderIds());
         });

         AJS.$("#update-email").click(function(e) {
            e.preventDefault();

            var updateRequest = AJS.$.ajax({
               url: "/rest/user/reminders",
               type: "POST",
               cache: false,
               dataType: "text", 
               data: JSON.stringify({
                  pids: getSelectedReminderIds()
               })
            });

            updateRequest.done(blank(reloadReminders));
         });

         AJS.$("#delete-reminders").click(function(e) {
            e.preventDefault();

            var deleteRequest = AJS.$.ajax({
               url: "/rest/user/reminders",
               type: "DELETE",
               cache: false,
               dataType: "text", 
               data: JSON.stringify({
                  pids: getSelectedReminderIds()
               })
            });

            deleteRequest.done(blank(reloadReminders));
         });
      };

      var blank = function(f) {
         // Blanks out all of the arguments that may be passed in
         return function() {
            f && f();
         };
      };

      // Give time for the page token to be put in place
      setTimeout(init, 1);
   });
});
