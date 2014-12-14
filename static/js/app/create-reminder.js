define([ "../lib/URI", "../host/request", "../lib/mustache", "../lib/moment-timezone", 'connect/panel-resize', 'connect/pagetoken', '../lib/anytime'],
   function(URI, HostRequest, Mustache, moment, RS) {
   require(['aui/form-validation']);

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

      var resetPanelHeight = function() {
         RS.resizeToBottomOf(AJS.$(".bottom-container"));
      };

      var createReminder = function(reminderDate, message) {
         setCreationState(creationState.creating);

         var userRequest = HostRequest.userDetails(userKey);
         var issueRequest = HostRequest.issueDetails(issueKey);

         return AJS.$.when(userRequest, issueRequest).then(function(userResponse, issueResponse) {
            var user = JSON.parse(userResponse[0]);
            var issue = JSON.parse(issueResponse[0]);

            var haskellFormat = "YYYY-MM-DDTHH:mm:ss.SSS";
            var requestData = {
               Issue: {
                   Key: issueKey,
                   Id: issueId,
                   Summary: issue.fields.summary,
                   ReturnUrl: window.location.href
               },
               User: {
                  Key: user.key,
                  Email: user.emailAddress
               },
               ReminderDate: reminderDate.utc().format(haskellFormat) + 'Z'
            };

            if(message) {
               requestData.Message = message;
            }

            var request = AJS.$.ajax({
               url: "/rest/reminder",
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
            url: "/rest/reminder",
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
               resetPanelHeight();
            }, 3000);
         }
      };

      var resetCreateForm = function() {
         AJS.$("#custom-reminder-magnitude").val("1");
         AJS.$("#custom-reminder-timeunit").val(timeUnits.day);
         AJS.$("#custom-reminder-message").val("");

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
            url: "/rest/reminders", 
            type: "GET", 
            cache: false,
            dataType: "text",
            data: {
               issueId: issueId
            }
         });

         currentUserRequest = HostRequest.userDetails(userKey);

         AJS.$.when(currentRemindersRequest, currentUserRequest).done(function(remindersResponse, userResponse) {
            currentRemindersRequest = null;
            currentUserRequest = null;

            var reminders = JSON.parse(remindersResponse[0]);
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
               AJS.$(".reminders .reminder").each(function() {
                  // We need to make sure that the tooltips don't cause the window to get larger vertically
                  // Or to scrunch up on one side of the screen. For that reason always make the tooltipl flow
                  // upwards (south) of the lozenge and if the reminder is left of center make it flow right (west) and
                  // vice versa.
                  var reminder = AJS.$(this);
                  var reminderCenter = reminder.position().left + (reminder.width() / 2);
                  var windowCenter = AJS.$(window).width() / 2;
                  var isLeft = reminderCenter < windowCenter;
                  reminder.tooltip({
                     aria: true,
                     title: setTooltipTitle,
                     gravity: isLeft ? "sw" : "se"
                  });
               });
            }

            // resize the container to compensate
            AP.resize();
         });
      };

      var genericErrorTimer;

      var handleGenericError = function(jqXHR) {
         var jsonError = JSON.parse(jqXHR.responseText);
         if(jsonError && jsonError.errorMessages) {
            // Clear the previous token
            if(genericErrorTimer) {
               clearTimeout(genericErrorTimer);
               genericErrorTimer = null;
            }

            // Just show the first error message because most of the time we just return one.
            AJS.$("#error-message").removeClass("hidden").find(".title").text(jsonError.errorMessages[0]);
            AP.resize();
            genericErrorTimer = setTimeout(function() {
               AJS.$("#error-message").addClass("hidden");
               resetPanelHeight();
            }, 10000);
         }
      };

      var init = function() {
         AJS.$(document).ajaxError(function(e, jqXHR) {
            handleGenericError(jqXHR);
         });

         setupTemplates();
         setTimeout(refreshReminders, 1); // So that the Acpt token has time to be injected

         // The gravity on this reminder is carefully selected to not make the panel expand.
         AJS.$("#reminder-help").tooltip({
            aria: true,
            title: setTooltipTitle,
            gravity: "w"
         });

         var fmt1 = '%m/%d/%Y';
         var fmt2 = '%h%p %i';
         var conv1 = new AnyTime.Converter({format:fmt1});
         var conv2 = new AnyTime.Converter({format:fmt2});
         var now = new Date();

         AJS.$("#remindDatePicker")[0].value = conv1.format(now);
         AJS.$("#remindDatePicker").AnyTime_picker({
            format: fmt1,
            hideInput: true,
            placement: "inline"
         });

         AJS.$("#remindTimePicker")[0].value = conv2.format(now);
         AJS.$("#remindTimePicker").AnyTime_picker({
            format: fmt2,
            hideInput: true,
            placement: "inline"
         });

         AJS.$('#create-reminder-form .custom-operations .submit').click(handle(function() {
            var input = AJS.$("#remindDatePicker").val() + ' ' + AJS.$("#remindTimePicker").val();
            var reminderDate = moment(input, "MM/DD/YYYY HHa mm");
            var message = AJS.$("#custom-reminder-message").val();

            // TODO check that the reminder date is vaild
            createReminder(reminderDate, message).done(resetCreateForm);
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

         var randomIntegerInRange = function(start, end) {
            var smallest = Math.min(start, end);
            var largest = Math.max(start, end);
            var difference = largest - smallest;
            return smallest + Math.floor(Math.random() * difference);
         };

         var setToMorningHour = function(date) {
            date.hours(randomIntegerInRange(6, 8)).minutes(randomIntegerInRange(0, 60));
            return date;
         };

         AJS.$("#add-reminder-tomorrow").click(handle(function() {
            createReminder(setToMorningHour(moment().add(1, 'days')));
         }));

         AJS.$("#add-reminder-next-week").click(handle(function() {
            createReminder(setToMorningHour(moment().add(7, 'days')));
         }));

         AJS.$(".custom-operations .cancel").click(handle(function() {
            resetCreateForm();
            resetPanelHeight();
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
