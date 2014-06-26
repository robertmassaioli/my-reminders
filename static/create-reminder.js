AJS.$(function() {
   AJS.log("Create reminder loaded...");
   var queryParams = URI(window.location.href).query(true);
   var issueId = parseInt(queryParams["issue_id"]);
   if(isNaN(issueId)) {
      console.log("This is an error and we need to block everything...");
   }
   AJS.log("Issue id: " + issueId);
   var userKey = queryParams["user_id"];

   var timeUnits = {
      minute: "Minute",
      hour: "Hour",
      day: "Day",
      week: "Week",
      year: "Year"
   };

   var requestUserDetails = function(userkey) {
      return AJS.$.Deferred(function() {
         var self = this;
         AP.request({ 
            url: "/rest/api/latest/user", 
            cache: true,
            data: { 
               key: userkey
            },
            success: self.resolve,
            fail: self.reject
         });
      });
   };
   
   var createReminder = function(timeDelay, timeUnit, message) {
      setCreationState(creationState.creating);

      var requestData = {
         IssueId: issueId,
         TimeDelay: timeDelay,
         TimeUnit: timeUnit
      };

      if(message) {
         requestData.Message = message;
      }

      var request = AJS.$.ajax({
         url: "/rest/ping",
         type: "PUT",
         cache: false,
         contentType: "application/json",
         data: JSON.stringify(requestData)
      });

      request.done(function() {
         setCreationState(creationState.created);
         refreshReminders();
      });

      request.fail(function() {
         setCreationState(creationState.failed);
      });

      return request;
   };

   var deleteReminder = function(reminderId) {
      var request = AJS.$.ajax({
         url: "/rest/ping",
         type: "DELETE",
         cache: false,
         data: {
            reminderId: reminderId
         }
      });

      request.done(refreshReminders);

      return request;
   };

   // Handle the event prevention and still fire off a handler function
   var handle = function(f) {
      return function(event) {
         event.preventDefault();

         f && f(event);
      };
   };

   var creationState = {
      notCreating: 0,
      creating: 1,
      created: 2,
      failed: 3
   };

   var timeoutHandle;

   var setCreationState = function(state) {
      var pending = AJS.$("#reminder-creation-pending");
      var successful = AJS.$("#reminder-creation-success");
      var failure = AJS.$("#reminder-creation-error");

      pending.toggleClass("hidden", state !== creationState.creating);
      successful.toggleClass("hidden", state !== creationState.created);
      failure.toggleClass("hidden", state !== creationState.failed);

      if(timeoutHandle) {
         clearTimeout(timeoutHandle);
         timeoutHandle = null;
      }

      if(state === creationState.created || state == creationState.failed) {
         timeoutHandle = setTimeout(function() {
            setCreationState(creationState.notCreating);
         }, 1500);
      }
   };

   var resetCreateForm = function() {
      AJS.$("#custom-ping-magnitude").val("1");
      AJS.$("#custom-ping-timeunit").val(timeUnits.day);
      AJS.$("#custom-ping-message").val("");

      showCustomCreate(false);
   };

   var templates = {};

   var setupTemplates = function() {
      templates.reminderLozenge = $('#reminder-lozenge').html();
      Mustache.parse(templates.reminderLozenge);
   };

   var refreshReminders = function() {
      var pingsRequest = AJS.$.ajax({
         url: "/rest/pings", 
         type: "GET", 
         cache: false,
         data: {
            issueId: issueId
         }
      });

      AJS.$.when(pingsRequest, requestUserDetails(userKey)).done(function(pingsResponse, userResponse) {
         var reminders = JSON.parse(pingsResponse[0]);
         var user = JSON.parse(userResponse[0]);

         // Clear the reminders container
         var remindersContainer = AJS.$("#upcoming-reminders");
         remindersContainer.empty();

         var haveReminders = reminders.length > 0;
         AJS.$("#no-reminders").toggleClass("hidden", haveReminders);
         AJS.$("#reminder-help").toggleClass("hidden", !haveReminders);
         if(haveReminders) {
            // Parse the dates in each of the reminders
            AJS.$.each(reminders, function(index, reminder) {
               var tzDate = moment(reminder["Date"]).tz(user.timeZone);
               reminder.momentDate = tzDate;
               reminder.fullDate = reminder.momentDate.format('D MMM YYYY hh:mmA');
               // Regex to follow the ADG: https://developer.atlassian.com/design/latest/foundations/dates/
               reminder.prettyDate = reminder.momentDate.fromNow().replace(/^in/, "In");
            });

            // Sort the reminders by date
            reminders.sort(function(a, b) {
               return a.momentDate.isBefore(b.momentDate) ? -1 : 1;
            });

            // Output the reminders
            AJS.$.each(reminders, function(index, reminder) {
               remindersContainer.append(Mustache.render(templates.reminderLozenge, reminder));
            });

            // Surface the messages in tooltips.
            AJS.$(".reminders .reminder").tooltip({
               aria: true,
               title: setTooltipTitle,
               gravity: "nw"
            });
         }

         // resize the container to compensate
         AP.resize();
      });
   };

   var init = function() {
      setupTemplates();
      setTimeout(refreshReminders, 1); // So that the Acpt token has time to be injected

      AJS.$("#reminder-help").tooltip({
         aria: true,
         title: setTooltipTitle,
         gravity: "nw"
      });

      AJS.$('#create-reminder-form .custom-operations .submit').click(handle(function() {
         var magnitude = parseInt(AJS.$("#custom-ping-magnitude").val());
         var timeUnit = AJS.$("#custom-ping-timeunit").val();
         if(timeUnit === "Month") {
            timeUnit = timeUnits.week;
            magnitude *= 4;
         }
         var message = AJS.$("#custom-ping-message").val();

         if(!isNaN(magnitude)) {
            createReminder(magnitude, timeUnit, message).done(resetCreateForm);
         }
      }));

      AJS.$(".reminders").on("click", ".reminder .aui-icon-close", function() {
         var $reminder = AJS.$(this).parent();
         var reminderId = $reminder.data("reminder-id");

         var deleteRequest = deleteReminder(reminderId);

         deleteRequest.done(function() {
            $reminder.remove();
         });

         deleteRequest.fail(function() {
            $reminder.addClass("error");
            setTimeout(function() {
               $reminder.removeClass("error");
            }, 1500);
         });
      });

      AJS.$("#add-reminder").click(handle(function() {
          showCustomCreate(true);
      }));

      AJS.$("#add-reminder-tomorrow").click(handle(function() {
         createReminder(1, timeUnits.day);
      }));

      AJS.$("#add-reminder-next-week").click(handle(function() {
         createReminder(1, timeUnits.week);
      }));

      AJS.$(".custom-operations .cancel").click(handle(function() {
         resetCreateForm();
      }));

      setCreationState(creationState.notCreating);
      resetCreateForm();
   };
   
   var setTooltipTitle = function() { 
      setTimeout(AP.resize, 2); // Dirty because tipsy lacks a callback function for after it displays an element.
      return this.getAttribute('original-title'); 
   };

   var showCustomCreate = function(showit) {
       AJS.$("#create-reminder-form").toggleClass("hidden", !showit);
       AJS.$("#add-reminder").attr("aria-pressed", showit);
       AP.resize();
   };

   init();
});
