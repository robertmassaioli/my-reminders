AJS.$(function() {
   console.log("Create reminder loaded...");
   
   var createPing = function(createData) {
      AJS.$.ajax({
         url: "/rest/ping",
         type: "GET",
         cache: false,
         contentType: "application/json"
      });

      return AJS.$.ajax({
         url: "/rest/ping",
         type: "PUT",
         cache: false,
         contentType: "application/json",
         data: JSON.stringify(createData)
      });
   };

   var init = function() {
      AJS.$('#create-reminder-form .custom-operations .submit').click(function(event) {
          event.preventDefault();

          createPing({
            pingMagnitude: 1,
            pingTimeUnit: "Day",
            pingIssueId: 10000,
            pingMessage: 'This is a message.',
          });

      });

      AJS.$("#add-reminder").click(function(event) {
          event.preventDefault();
          showCustomCreate(true);
      });

      AJS.$(".custom-operations .cancel").click(function() {
          event.preventDefault();
          showCustomCreate(false);
      });

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
