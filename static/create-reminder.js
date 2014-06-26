AJS.$(function() {
   AJS.log("Create reminder loaded...");
   var queryParams = URI(window.location.href).query(true);
   var issueId = parseInt(queryParams["issue_id"]);
   if(isNaN(issueId)) {
      console.log("This is an error and we need to block everything...");
   }
   AJS.log("Issue id: " + issueId);

   var timeUnits = {
      minute: "Minute",
      hour: "Hour",
      day: "Day",
      week: "Week",
      year: "Year"
   };
   
   var createPing = function(createData) {
      setCreationState(creationState.creating);

      AJS.$.ajax({
         url: "/rest/ping",
         type: "GET",
         cache: false
      });

      var request = AJS.$.ajax({
         url: "/rest/ping",
         type: "PUT",
         cache: false,
         contentType: "application/json",
         data: JSON.stringify(createData)
      });

      request.done(function() {
         setCreationState(creationState.created);
      });

      request.fail(function() {
         setCreationState(creationState.failed);
      });

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
         }, 4000);
      }
   };

   var resetCreateForm = function() {
      AJS.$("#custom-ping-magnitude").val("1");
      AJS.$("#custom-ping-timeunit").val(timeUnits.day);
      AJS.$("#custom-ping-message").val("");

      showCustomCreate(false);
   };

   var init = function() {
      AJS.$('#create-reminder-form .custom-operations .submit').click(handle(function() {
         var magnitude = parseInt(AJS.$("#custom-ping-magnitude").val());
         var timeUnit = AJS.$("#custom-ping-timeunit").val();
         if(timeUnit === "Month") {
            timeUnit = timeUnits.week;
            magnitude *= 4;
         }
         var message = AJS.$("#custom-ping-message").val();

         if(!isNaN(magnitude)) {
            var request = createPing({
               pingMagnitude: magnitude,
               pingTimeUnit: timeUnit,
               pingIssueId: issueId,
               pingMessage: message,
            });

            request.done(resetCreateForm);
         }
      }));

      AJS.$("#add-reminder").click(handle(function() {
          showCustomCreate(true);
      }));

      AJS.$("#add-reminder-tomorrow").click(handle(function() {
         createPing({
            pingMagnitude: 1,
            pingTimeUnit: timeUnits.day,
            pingIssueID: issueId
         });
      }));

      AJS.$("#add-reminder-next-week").click(handle(function() {
         createPing({
            pingMagnitude: 1,
            pingTimeUnit: timeUnits.week,
            pingIssueID: issueId
         });
      }));

      AJS.$(".custom-operations .cancel").click(handle(function() {
         resetCreateForm();
      }));

      AJS.$(".reminders .reminder").tooltip({
         aria: true,
         title: setTooltipTitle
      });

      setCreationState(creationState.notCreating);
      resetCreateForm();
   };
   
   var setTooltipTitle = function() { 
      setTimeout(AP.resize, 2); // Dirty because tipsy lacks a callback function.
      return this.getAttribute('original-title'); 
   };

   var showCustomCreate = function(showit) {
       AJS.$("#create-reminder-form").toggleClass("hidden", !showit);
       AJS.$("#add-reminder").attr("aria-pressed", showit);
       AP.resize();
   };

   init();
});
