module WebhookHandlers 
   ( handleIssueUpdateWebhook
   , handleIssueDeleteWebhook
   ) where

import           Application
import qualified Snap.Core as SC
import           SnapHelpers

handleIssueUpdateWebhook :: AppHandler ()
handleIssueUpdateWebhook = handleMethods [(SC.POST, handleUpdate)]

handleUpdate :: AppHandler ()
handleUpdate = undefined

handleIssueDeleteWebhook :: AppHandler ()
handleIssueDeleteWebhook = handleMethods [(SC.POST, handleDelete)]

handleDelete :: AppHandler ()
handleDelete = undefined
