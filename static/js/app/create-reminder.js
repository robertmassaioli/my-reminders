require(['aui/form-validation']);

define(function(require) {
   // Define and then require trick to get around this problem:
   // http://stackoverflow.com/q/12574255/83446
   var URI = require("URI"),
       HostRequest = require("host/request"),
       Mustache = require("mustache"),
       moment = require("moment-timezone");

   AJS.$.fn.flashClass = function(c, userOptions) {
      var defaults = {
         starton: true,
         timeout: 1500,
         finishedCallback: null
      };
      var options = AJS.$.extend({}, defaults, userOptions);

      var self = this;
      self.toggleClass(c, options.starton);
      return setTimeout(function() { 
         self.toggleClass(c, !options.starton); 
         options.finishedCallback && options.finishedCallback();
      }, options.timeout);
   };

   AJS.$(function() {
      AJS.log("Create reminder loaded...");
      var queryParams = URI(window.location.href).query(true);
      var issueId = parseInt(queryParams["issue_id"]);
      if(isNaN(issueId)) {
         throw "No issue id on the page. Cannot do anything.";
      }
      var issueKey = queryParams["issue_key"];
      var userKey = queryParams["user_id"];

      var timeUnits = {
         minute: "Minute",
         hour: "Hour",
         day: "Day",
         week: "Week",
         year: "Year"
      };

      var createReminder = function(timeDelay, timeUnit, message) {
         setCreationState(creationState.creating);

         var userRequest = HostRequest.userDetails(userKey);
         var issueRequest = HostRequest.issueDetails(issueKey);

         return AJS.$.when(userRequest, issueRequest).then(function(userResponse, issueResponse) {
            var user = JSON.parse(userResponse[0]);
            var issue = JSON.parse(issueResponse[0]);
            var requestData = {
               Issue: {
                   Key: issueKey,
                   Id: issueId,
                   Summary: issue.fields.summary,
                   ReturnUrl: window.location.href
               },
               User: {
                  Key: user.key,
                  Email: user.emailAddress,
               },
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
               dataType: "text", 
               data: JSON.stringify(requestData)
            });

            request.done(function() {
               setCreationState(creationState.created, user.emailAddress);
               refreshReminders();
            });

            request.fail(function() {
               setCreationState(creationState.failed);
            });

            return request;
         });
         

      };

      var deleteReminder = function(reminderId) {
         var request = AJS.$.ajax({
            url: "/rest/ping",
            type: "DELETE",
            cache: false,
            dataType: "text",
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

      var setCreationState = function(state, emailAddress) {
         var pending = AJS.$("#reminder-creation-pending");
         var failure = AJS.$("#reminder-creation-error");

         pending.toggleClass("hidden", state !== creationState.creating);
         if(emailAddress) AJS.$("#success-message .email").text(emailAddress);
         AJS.$("#success-message").toggleClass("hidden", state !== creationState.created); 
         failure.toggleClass("hidden", state !== creationState.failed);

         if(timeoutHandle) {
            clearTimeout(timeoutHandle);
            timeoutHandle = null;
         }

         if(state === creationState.created || state == creationState.failed) {
            timeoutHandle = setTimeout(function() {
               setCreationState(creationState.notCreating);
            }, 3000);
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

      var currentRemindersRequest;
      var currentUserRequest;

      var refreshReminders = function() {
         if(currentRemindersRequest) {
            currentRemindersRequest.abort();
            currentRemindersRequest = null;
         }

         if(currentUserRequest) {
            currentUserRequest.abort();
            currentUserRequest = null;
         }

         currentRemindersRequest = AJS.$.ajax({
            url: "/rest/pings", 
            type: "GET", 
            cache: false,
            dataType: "text",
            data: {
               issueId: issueId
            }
         });

         currentUserRequest = HostRequest.userDetails(userKey);

         AJS.$.when(currentRemindersRequest, currentUserRequest).done(function(pingsResponse, userResponse) {
            currentRemindersRequest = null;
            currentUserRequest = null;

            var reminders = JSON.parse(pingsResponse[0]);
            var user = JSON.parse(userResponse[0]);

            console.log(reminders);
            // Clear the reminders container
            var remindersContainer = AJS.$("#upcoming-reminders");
            remindersContainer.empty();

            var haveReminders = reminders.length > 0;
            AJS.$("#no-reminders").toggleClass("hidden", haveReminders);
            AJS.$("#reminder-help").toggleClass("hidden", !haveReminders);
            if(haveReminders) {
               // Parse the dates in each of the reminders
               AJS.$.each(reminders, function(index, reminder) {
                  var date = moment(reminder["Date"]);
                  var tzDate = date.tz(user.timeZone);
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

      var genericErrorTimer;

      var handleGenericError = function(jqXHR) {
         var jsonError = JSON.parse(jqXHR.responseText);
         console.log("Error response: ", jsonError);
         if(jsonError && jsonError.errorMessage) {
            // Clear the previous token
            if(genericErrorTimer) {
               clearTimeout(genericErrorTimer);
               genericErrorTimer = null;
            }

            AJS.$("#error-message").removeClass("hidden").find(".title").text(jsonError.errorMessage);
            AP.resize();
            genericErrorTimer = setTimeout(function() {
               AJS.$("#error-message").addClass("hidden");
               AP.resize();
            }, 10000);
         }
      };

      var init = function() {
         AJS.$(document).ajaxError(function(e, jqXHR) {
            handleGenericError(jqXHR);
         });

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
               $reminder.mouseout(); // IMPORTANT: This mouseout is the solution to: https://github.com/jaz303/tipsy/issues/19
               $reminder.remove();
            });

            deleteRequest.fail(function() {
               $reminder.flashClass("error");
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
         setTimeout(AP.resize, 1); // Dirty because tipsy lacks a callback function for after it displays an element.
         return this.getAttribute('original-title'); 
      };

      var showCustomCreate = function(showit) {
          AJS.$("#create-reminder-form").toggleClass("hidden", !showit);
          AJS.$("#add-reminder").attr("aria-pressed", showit);
          AP.resize();
      };

      init();
   });
});
