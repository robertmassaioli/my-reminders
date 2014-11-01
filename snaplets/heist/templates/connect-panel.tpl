<!DOCTYPE html>
<html lang="en">
   <head profile="http://www.w3.org/2005/10/profile">
      <link rel="icon" type="image/ico" href="/static/images/favicon.v3.ico" />

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

      <apply template="headers/requirejs" />

      <apply template="headers/tenant" />

      <apply template="headers/aui" />
      <apply template="headers/aui-experimental" />

      <!--apply template="headers/jquery-url-parser" /-->

      <apply template="headers/visibility" />

      <apply template="headers/moment" />

      <header-extra />
   </head>
   <body>
      <apply-content />
   </body>
</html>
