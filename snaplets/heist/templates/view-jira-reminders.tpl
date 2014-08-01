<bind tag="js">
   <script type="text/javascript" src="${href}"></script>
</bind>

<bind tag="stylesheet">
   <link rel="stylesheet" type="text/css" href="${href}">
</bind>

<apply template="connect-panel">
   <bind tag="header-extra">
      <js><bind tag="href">/static/host-request.js</bind></js>
      <js><bind tag="href">/static/view-my-reminders.js</bind></js>
      <stylesheet><bind tag="href">/static/view-my-reminders.css</bind></stylesheet>

      <js><bind tag="href">//cdnjs.cloudflare.com/ajax/libs/URI.js/1.11.2/URI.min.js</bind></js>
      <js><bind tag="href">//cdnjs.cloudflare.com/ajax/libs/mustache.js/0.8.1/mustache.min.js</bind></js>
      <js><bind tag="href">/static/moment-timezone-with-data-2010-2020.js</bind></js>

      <script id="reminder-row" type="x-tmpl-mustache">
         <tr class="reminder" data-reminder-id="{{PingId}}">
            <td><input type="checkbox" class="select" /></td>
            <td>{{fullDate}}</td>
            <td><a href="{{{issueLink}}}">[{{IssueKey}}] {{IssueSummary}}</a></td>
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
             <button id="update-email" class="aui-button"><span class="aui-icon aui-icon-small aui-iconfont-build"></span> Update email</button>
             <button id="delete-reminders" class="aui-button"><span class="aui-icon aui-icon-small aui-iconfont-delete">Delete</span> Delete</button>
         </p>

         <table id="reminders" class="aui">
             <thead>
                 <tr>
                     <th><input type="checkbox" id="master-selector" /></th>
                     <th>Date</th>
                     <th>Issue</th>
                     <th>Email</th>
                     <th>Message</th>
                 </tr>
             </thead>
             <tbody class="container">
             </tbody>
         </table>
         <!-- TODO have a message here in case no reminders exist -->
      </div>
   </section></div></div>
</apply>
