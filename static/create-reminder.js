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
      AJS.$.ajax({
         url: "/rest/ping",
         type: "GET",
         cache: false
      });

      return AJS.$.ajax({
         url: "/rest/ping",
         type: "PUT",
         cache: false,
         contentType: "application/json",
         data: JSON.stringify(createData)
      });
   };

   // Handle the event prevention and still fire off a handler function
   var handle = function(f) {
      return function(event) {
         event.preventDefault();

         f && f(event);
      };
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
            createPing({
               pingMagnitude: magnitude,
               pingTimeUnit: timeUnit,
               pingIssueId: issueId,
               pingMessage: message,
            });
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
          showCustomCreate(false);
      }));

      AJS.$(".reminders .reminder").tooltip({
         aria:true,
         title: setTooltipTitle
      });
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
