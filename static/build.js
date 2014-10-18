({
   'appDir': 'js',
   'mainConfigFile': 'js/common.js',
   'dir': 'built',
   'modules': [
      {
         'name': '../common',
         'include': [
            'host/request.js'
         ]
      },
      {
         'name': 'app/create-reminder',
         'exclude': ['../common']
      },
      {
         'name': 'app/view-my-reminders',
         'exclude': ['../common']
      }
   ]
})
