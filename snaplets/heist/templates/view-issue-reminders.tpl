<apply template="connect-panel">
   <bind tag="body-class">view-issue-reminders</bind>
   <bind tag="header-extra">
      <js src="/static/${resourcesVersion}/js/app/view-issue-reminders.js" />
      <script>require(['app/view-issue-reminders']);</script>

      <css href="/static/${resourcesVersion}/css/app.css" />

      <script id="reminder-lozenge" type="x-tmpl-mustache">
         <span data-reminder-id="{{ReminderId}}" title="{{#Message}}{{Message}} ({{/Message}}{{fullDate}}{{#Message}}){{/Message}} [{{UserEmail}}]" class="reminder aui-label aui-label-closeable">{{prettyDate}}<span tabindex="0" class="aui-icon aui-icon-close" original-title="(remove closableNoUrl)">(remove closableNoUrl)</span></span>
      </script>
   </bind>

   <!-- TODO work out per user which one gets pressed the most and optimise for that -->
   <!-- TODO textinput instead of textarea because textinputs can have a maxlength -->
   <div class="reminder-actions">
       <div id="split-button-demo" class="aui-buttons"><button id="add-reminder" class="aui-button aui-button-split-main"><span class="aui-icon aui-icon-small aui-iconfont-add">Add</span> Reminder</button><button id="add-reminder-more" class="aui-button aui-dropdown2-trigger aui-button-split-more" aria-owns="split-container-dropdown" aria-haspopup="true">Split button more</button></div>
       <div id="split-container-dropdown" class="aui-dropdown2 aui-style-default" data-container="split-button-demo">
           <ul class="aui-list-truncate">
               <li><a id="add-reminder-tomorrow"    href="#">Tomorrow</a></li>
               <li><a id="add-reminder-next-week"   href="#">In a week</a></li>
               <li><a id="add-reminder-next-month"  href="#">In a month</a></li>
               <li><a id="add-custom-reminder"      href="#">Select a time...</a></li>
           </ul>
       </div>
      <span class="status-container">
           <span id="reminder-creation-pending" class="aui-icon aui-icon-wait">Wait</span>
           <span id="reminder-creation-error"   class="aui-icon aui-icon-error">Error</span>
       </span>
   </div>
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
