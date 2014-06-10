<!DOCTYPE html>
<html lang="en">
   <head>
      <meta charset="utf-8" />
      <meta version="${version}" />
      <meta name="avatarSize" content="${avatarSize}" />
      <meta name="hostId" content="${hostId}" />    
      <meta name="resourceId" content="${resourceId}" />
      <meta name="userId" content="${userId}" />
      <meta name="acpt" content="${acpt}">
      <meta content="IE=EDGE" http-equiv="X-UA-Compatible" /> 

      <!-- TODO add static versions of all of the resources here so that
         - the plugin will still work in development without an internet
         - connection. This requires understanding the snap template language.
         - Also, it would be great if there was a Vim plugin for the Heist
         - template language.
         -->

      <apply template="headers/tenant" />

      <apply template="headers/aui" />

      <apply template="headers/jquery-url-parser" />

      <apply template="headers/visibility" />

      <apply template="headers/moment" />

      <!-- TODO this will never be served because we do not have code like this... -->
      <link rel="stylesheet" type="text/css" href="/stylesheets/main.css" />
      <script type="text/javascript" src="/js/pagetoken.js"></script>
   </head>
   <body>
      <apply-content />
   </body>
</html>
