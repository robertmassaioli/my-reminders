<bind tag="js">
   <script type="text/javascript" src="${href}"></script>
</bind>

<bind tag="stylesheet">
   <link rel="stylesheet" type="text/css" href="${href}">
</bind>

<apply template="connect-panel">
   <bind tag="header-extra">
      <js><bind tag="href">/static/create-reminder.js</bind></js>
      <stylesheet><bind tag="href">/static/create-reminder.css</bind></stylesheet>

      <js><bind tag="href">//cdnjs.cloudflare.com/ajax/libs/URI.js/1.11.2/URI.min.js</bind></js>
      <js><bind tag="href">//cdnjs.cloudflare.com/ajax/libs/mustache.js/0.8.1/mustache.min.js</bind></js>
      <js><bind tag="href">/static/moment-timezone-with-data-2010-2020.js</bind></js>

      <script id="reminder-lozenge" type="x-tmpl-mustache">
         <span data-reminder-id="{{PingId}}" title="{{#Message}}{{Message}} ({{/Message}}{{fullDate}}{{#Message}}){{/Message}} [{{Email}}]" class="reminder aui-label aui-label-closeable">{{prettyDate}}<span tabindex="0" class="aui-icon aui-icon-close" original-title="(remove closableNoUrl)">(remove closableNoUrl)</span></span>
      </script>
   </bind>

   <!-- TODO work out per user which one gets pressed the most and optimise for that -->
   <!-- TODO add AUI validation, think about validating time -->
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
               <label for="d-fname">When<span class="aui-icon icon-required"> required</span></label>
               <input id="custom-ping-magnitude" class="text short-field" type="text" title="magnitude" value="1">
               <select id="custom-ping-timeunit" class="select short-field" title="time unit">
                   <option value="Day" selected="selected">Day</option>
                   <option value="Week">Week</option>
                   <option value="Month">Month</option>
                   <option value="Year">Year</option>
               </select>
               <div class="description">The time when you should be notified</div>
           </div>
           <div class="field-group custom-container">
               <label for="licenseKey">Message</label>
               <textarea id="custom-ping-message" class="textarea" rows="3" cols="10"></textarea>
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
   <div class="reminders-container">
      <div>Upcoming reminders: <span id="no-reminders">None yet; create some.</span><span id="reminder-help" title="Hover over the reminders for more information." class="aui-icon aui-icon-info">Info</span></div>
      <div id="upcoming-reminders" class="reminders"></div>
   </div>
   <div id="error-message" class="aui-message error hidden">
       <p class="title">
           <span class="aui-icon icon-error"></span>
           <strong class="title">Destructive operation!</strong>
       </p>
   </div>
   <div id="success-message" class="aui-message success hidden">
      <p class="title">
         <span class="aui-icon icon-success"></span>
         <strong class="title">Reminder set for <span class="email"></span></strong>
      </p>
   </div>
</apply>
