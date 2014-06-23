AJS.$(function() {
   console.log("Create reminder loaded...");
   
   var createPing = function(createData) {
      return AJS.$.ajax({
         url: "/rest/ping",
         type: "PUT",
         cache: false,
         dataType: "json",
         data: createData
      });
   };

   var init = function() {
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
