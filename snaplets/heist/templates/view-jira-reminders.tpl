<bind tag="js">
   <script type="text/javascript" src="${href}"></script>
</bind>

<bind tag="stylesheet">
   <link rel="stylesheet" type="text/css" href="${href}">
</bind>

<apply template="connect-panel">
   <bind tag="header-extra">
      <js><bind tag="href">/static/view-reminders.js</bind></js>
      <stylesheet><bind tag="href">/static/view-reminders.css</bind></stylesheet>

      <js><bind tag="href">//cdnjs.cloudflare.com/ajax/libs/URI.js/1.11.2/URI.min.js</bind></js>
      <js><bind tag="href">//cdnjs.cloudflare.com/ajax/libs/mustache.js/0.8.1/mustache.min.js</bind></js>
      <js><bind tag="href">/static/moment-timezone-with-data-2010-2020.js</bind></js>

      <script id="reminder-row" type="x-tmpl-mustache">
         <tr class="reminder" data-reminder-id="{{PingId}}">
            <td><input type="checkbox" class="select" /></td>
            <td>{{prettyDate}}</td>
            <td><a href="{{issueLink}}">[{{IssueKey}}] {{IssueSubject}}</a></td>
            <td>{{Email}}</td>
         </tr>
      </script>
   </bind>

   <h2>My reminders</h2>

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
           </tr>
       </thead>
       <tbody class="container">
           <tr class="reminder">
               <td><input type="checkbox" class="select" /></td>
               <td headers="basic-number">31 Jul 2014 05:49PM </td>
               <td headers="basic-fname"><a href="/some/link">[DEMO-1]  What is an issue?</a></td>
               <td>robertmassaioli@gmail.com</td>
           </tr>
           <tr class="reminder">
               <td><input type="checkbox" class="select" /></td>
               <td headers="basic-number">31 Jul 2014 05:49PM </td>
               <td headers="basic-fname"><a href="/some/link">[DEMO-1]  What is an issue?</a></td>
               <td>robertmassaioli@gmail.com</td>
           </tr>
           <tr class="reminder">
               <td><input type="checkbox" class="select" /></td>
               <td headers="basic-number">31 Jul 2014 05:49PM </td>
               <td headers="basic-fname"><a href="/some/link">[DEMO-1]  What is an issue?</a></td>
               <td>robertmassaioli@gmail.com</td>
           </tr>
       </tbody>
   </table>
</apply>
