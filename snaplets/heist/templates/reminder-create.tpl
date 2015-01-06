<apply template="connect-panel">
   <bind tag="header-extra">
      <js src="/static/${resourcesVersion}/js/app/create-reminder.js" />
      <script>require(['app/create-reminder']);</script>

      <css href="/static/${resourcesVersion}/css/create-reminder.css" />

      <script id="reminder-lozenge" type="x-tmpl-mustache">
         <span data-reminder-id="{{ReminderId}}" title="{{#Message}}{{Message}} ({{/Message}}{{fullDate}}{{#Message}}){{/Message}} [{{UserEmail}}]" class="reminder aui-label aui-label-closeable">{{prettyDate}}<span tabindex="0" class="aui-icon aui-icon-close" original-title="(remove closableNoUrl)">(remove closableNoUrl)</span></span>
      </script>
   </bind>

   <!-- TODO work out per user which one gets pressed the most and optimise for that -->
   <!-- TODO textinput instead of textarea because textinputs can have a maxlength -->
   <!-- TODO Test showing the form permenantly -->
   <div class="reminder-actions aui-buttons">
      <button id="add-reminder" class="aui-button"><span class="aui-icon aui-icon-small aui-iconfont-add">Add</span> Reminder</button>
      <button id="add-reminder-tomorrow" class="aui-button aui-button-subtle">Tomorrow</button>
      <button id="add-reminder-next-week" class="aui-button aui-button-subtle">In a week</button>
      <span class="status-container">
           <span id="reminder-creation-pending" class="aui-icon aui-icon-wait">Wait</span>
           <span id="reminder-creation-error"   class="aui-icon aui-icon-error">Error</span>
       </span>
   </div>
   <form action="#" method="post" id="create-reminder-form" class="aui hidden">
       <fieldset>
           <div class="field-group custom-container">
               <label for="custom-reminder-magnitude">When<span class="aui-icon icon-required"> required</span></label>
               <input id="custom-reminder-magnitude" class="text short-field" type="text" title="magnitude" value="1" data-aui-validation-field data-aui-validation-pattern="[0-9]+" data-aui-validation-pattern-msg="Must be a whole number ({0}).">
               <select id="custom-reminder-timeunit" class="select short-field" title="time unit">
                   <option value="Day" selected="selected">Day</option>
                   <option value="Week">Week</option>
                   <option value="Month">Month</option>
                   <option value="Year">Year</option>
               </select>
               <div class="description">The time when you should be notified</div>
           </div>
           <div class="field-group custom-container">
               <label for="licenseKey">Message</label>
               <textarea id="custom-reminder-message" class="textarea" rows="3" cols="10"></textarea>
               <div class="description">A short optional reminder mesage</div>
           </div>
       </fieldset>
       <div class="buttons-container custom-container custom-operations">
           <div class="buttons">
               <input class="button submit" type="submit" value="Add" id="d-save-btn1">
               <a class="cancel" href="#">Close</a>
           </div>
       </div>
   </form>
   <div class="reminders-container bottom-container">
      <div>Upcoming reminders: <span id="no-reminders">None yet; create some.</span><span id="reminder-help" title="Hover over the reminders for more information." class="aui-icon aui-icon-small aui-iconfont-info">Info</span></div>
      <div id="upcoming-reminders" class="reminders"></div>
   </div>
   <div id="error-message" class="aui-message aui-message-warning hidden">
       <p class="title">
           <span class="aui-icon icon-error"></span>
           <strong class="title">Loading error message...</strong>
       </p>
   </div>
   <div id="success-message" class="aui-message aui-message-success hidden">
      <p class="title">
         <span class="aui-icon icon-success"></span>
         <strong class="title">Reminder set for <span class="email"></span></strong>
      </p>
   </div>
</apply>
