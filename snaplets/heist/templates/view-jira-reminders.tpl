<apply template="connect-panel">
   <bind tag="header-extra">
      <js src="/static/${resourcesVersion}/js/app/view-my-reminders.js" />
      <script>require(['app/view-my-reminders']);</script>

      <css href="/static/${resourcesVersion}/css/view-my-reminders.css" />

      <script id="reminder-row" type="x-tmpl-mustache">
         <tr class="reminder" data-reminder-id="{{ReminderId}}">
            <td><input type="checkbox" class="select" /></td>
            <td>{{fullDate}}</td>
            <td><a href="{{{issueLink}}}" target="_parent">[{{IssueKey}}] {{IssueSummary}}</a></td>
            <td>{{UserEmail}}</td>
            <td>{{Message}}</td>
         </tr>
      </script>
   </bind>

   <div class="aui-page-panel atlassian-connect-infinite-expand-fix"><div class="aui-page-panel-inner"><section class="aui-page-panel-content">
      <h2>My reminders</h2>

      <div id="not-logged-in" class="aui-message aui-message-error hidden">
          <p class="title">
              <strong>You are not logged in</strong>
          </p>
          <p>Your reminders cannot be loaded until you login.</p>
      </div>

      <div id="logged-in-content">
         <p>
            Bulk Update:
            <button id="update-email" class="aui-button">Update email address</button>
            <button id="delete-reminders" class="aui-button">Delete</button>
         </p>

         <!-- TODO When https://ecosystem.atlassian.net/browse/AUI-2846 is resolved then please make this table use aui-table-sortable -->
         <table id="reminders" class="aui">
             <thead>
                 <tr>
                     <th class="aui-table-column-unsortable"><input type="checkbox" id="master-selector" /></th>
                     <th>Date</th>
                     <th>Issue</th>
                     <th>Email</th>
                     <th class="aui-table-column-unsortable">Message</th>
                 </tr>
             </thead>
             <tbody class="container">
             </tbody>
         </table>
         <!-- TODO have a message here in case no reminders exist -->
      </div>
   </section></div></div>
</apply>
