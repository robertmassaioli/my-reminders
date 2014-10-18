//The build will inline common dependencies into this file.

//For any third party dependencies, like jQuery, place them in the lib folder.

//Configure loading modules from the lib directory,
//except for 'app' ones, which are in a sibling
//directory.
requirejs.config({
   'baseUrl': '/static/built',
   'paths': {
      'app': '../app',
      'connect': '../connect',
      'host': '../host'
   },
   'shim': {
      'aui': {
         'deps': ['jquery'],
         'exports': 'AJS'
      }
   }
});

require(['connect/pagetoken']);
