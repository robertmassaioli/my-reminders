<!DOCTYPE html>
<html lang="en">
   <head>
      <meta charset="utf-8" />
      <meta version="${version}" />
      <meta name="pollIntervalSeconds" content="${pollerInterval}" />
      <meta name="avatarSize" content="${avatarSize}" />
      <meta name="hostId" content="${hostId}" />    
      <meta name="resourceId" content="${resourceId}" />
      <meta name="userId" content="${userId}" />
      <meta name="acpt" content="${acpt}">
      <meta content="IE=EDGE" http-equiv="X-UA-Compatible" /> 

      <!-- JQuery -->
      <script src="//ajax.googleapis.com/ajax/libs/jquery/1.8.3/jquery.min.js"></script>

      <!-- CDN AUI -->
      <link rel="stylesheet" href="//cdnjs.cloudflare.com/ajax/libs/aui/5.2/css/aui.css" media="all">
      <!--[if lt IE 9]><link rel="stylesheet" href="//cdnjs.cloudflare.com/ajax/libs/aui/5.2/css/aui-ie.css" media="all"><![endif]-->
      <!--[if IE 9]><link rel="stylesheet" href="//cdnjs.cloudflare.com/ajax/libs/aui/5.2/css/aui-ie9.css" media="all"><![endif]-->
      <script src="//cdnjs.cloudflare.com/ajax/libs/aui/5.2/js/aui.js"></script>
      <!--[if lt IE 9]><script src="//cdnjs.cloudflare.com/ajax/libs/aui/5.2/js/aui-ie.js"></script><![endif]-->

      <!-- JQuery URL Parser - TODO Required? -->
      <script src="//cdnjs.cloudflare.com/ajax/libs/jquery-url-parser/2.3.1/purl.min.js"></script>

      <!-- Visibility - TODO Required? -->
      <script src="//cdnjs.cloudflare.com/ajax/libs/visibility.js/0.6.2/visibility.min.js"></script>

      <!-- Moment.js - TODO Probably replace with Sugar.js -->
      <script src="//cdnjs.cloudflare.com/ajax/libs/moment.js/2.1.0/moment.min.js"></script>
      <link rel="stylesheet" type="text/css" href="/stylesheets/main.css" />

      <!-- TODO get the acHostBaseUrl in here -->
      <meta name="hostBaseUrl" content="${acHostBaseUrl}" />
      <script type="text/javascript" src="${acHostBaseUrl}/atlassian-connect/all.js"></script>
      <link rel="stylesheet" type="text/css" href="${acHostBaseUrl}/atlassian-connect/all.css"/>        
      <script type="text/javascript" src="/js/pagetoken.js"></script>
   </head>
   <body>
      <h1>Test Panel</h1>
      <p>Create your pings in here...</p>
   </body>
</html>
