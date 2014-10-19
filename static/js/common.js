//The build will inline common dependencies into this file.

//For any third party dependencies, like jQuery, place them in the lib folder.

//Configure loading modules from the lib directory,
//except for 'app' ones, which are in a sibling
//directory.
require.config({
   'baseUrl': '/static/js/lib',
   'paths': {
      'app': '../app',
      'connect': '../connect',
      'host': '../host',
      'moment': '//cdnjs.cloudflare.com/ajax/libs/moment.js/2.8.3/moment.min',
      'mustache': '//cdnjs.cloudflare.com/ajax/libs/mustache.js/0.8.1/mustache.min'
   },
   'shim': {
      'aui': {
         'deps': ['jquery'],
         'exports': 'AJS'
      }
   }
});

require(['connect/pagetoken']);
