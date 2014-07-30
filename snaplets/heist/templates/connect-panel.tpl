<!DOCTYPE html>
<html lang="en">
   <head>
      <meta charset="utf-8" />
      <meta version="${version}" />
      <meta name="avatarSize" content="${avatarSize}" />
      <meta name="hostId" content="${hostId}" />    
      <meta name="resourceId" content="${resourceId}" />
      <meta name="userKey" content="${userKey}" />
      <hasSplice name="connectPageToken">
      <meta name="acpt" content="${connectPageToken}">
      </hasSplice>
      <meta content="IE=EDGE" http-equiv="X-UA-Compatible" /> 

      <!-- TODO add static versions of all of the resources here so that
         - the plugin will still work in development without an internet
         - connection. This requires understanding the snap template language.
         - Also, it would be great if there was a Vim plugin for the Heist
         - template language.
         -->

      <apply template="headers/tenant" />

      <apply template="headers/aui" />
      <apply template="headers/aui-experimental" />

      <apply template="headers/jquery-url-parser" />

      <apply template="headers/visibility" />

      <apply template="headers/moment" />

      <header-extra />

      <!-- TODO this will never be served because we do not have code like this... -->
      <link rel="stylesheet" type="text/css" href="/stylesheets/main.css" />
   </head>
   <body>
      <apply-content />
      <hasSplice name="connectPageToken">
      <script type="text/javascript">(window.AJS && window.AJS.$) || document.write("<script type='text/javascript' src='//code.jquery.com/jquery-1.10.2.min.js'>\x3C/script>")</script>
      <script type="text/javascript" src="/static/pagetoken.js"></script>
      </hasSplice>
   </body>
</html>
