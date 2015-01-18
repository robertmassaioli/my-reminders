define(['moment', '../lib/jquery.datetimepicker'], function(moment) {
    "use strict";

    // Handle the event prevention and still fire off a handler function
    var handle = function(f) {
        return function(event) {
            event.preventDefault();

            f && f(event);
        };
    };

    AJS.$(function() {
        // TODO init the datepicker
        AJS.$("#reminderDate").datetimepicker();

        AP.require(['dialog', 'events'], function(dialog, events){
            AJS.log("Dialog: Loaded the create reminder dialog.");
            AJS.log(events);
            events.emit('dialog.loaded');

            // Load tomorrow into the box
            var startTime = moment().add(1, 'days').hours(7).minutes(0).seconds(0);
            var timeFormat = "YYYY/MM/DD HH:mm:ss";
            AJS.$("#reminderDate").val(startTime.format(timeFormat));

            // Bind the add button
            AJS.$("#button-add").click(handle(function() {
                // TODO emit the data that the other code will need
                AJS.log("Dialog: Clicked the add button in the dialog.");
                var createData = {
                    dueDate: AJS.$("#reminderDate").val(),
                    message: AJS.$("#reminderMessage").val()
                };
                AJS.log(createData);
                events.emit('add-reminder-submit', createData);
            }));
            // Bind the close button
            AJS.$("#button-close").click(handle(function() {
                AJS.log("Dialog: Clicked the close button in the dialog.");
                events.emit('add-reminder-cancelled');
                dialog.close();
            }));
        });
    });
});
