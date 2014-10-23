module WebhookHandlers 
   ( handleIssueUpdateWebhook
   , handleIssueDeleteWebhook
   ) where

import           Application
import qualified Connect.AtlassianTypes   as AT
import qualified Connect.Tenant           as CT
import           Data.Maybe (fromMaybe)
import qualified Snap.Core                as SC
import           SnapHelpers
import qualified WithToken                as WT

handleIssueUpdateWebhook :: AppHandler ()
handleIssueUpdateWebhook = handleMethods [(SC.POST, WT.tenantFromToken handleUpdate)]

handleUpdate :: CT.ConnectTenant -> AppHandler ()
handleUpdate (tenant, _) = do
   -- Parse the Webhook JSON message
   request <- SC.readRequestBody (1024 * 10) -- TODO this magic number is crappy, improve
   case parseUpdatedKey request of
      Nothing -> respondNoContent 
      Just update -> do
         -- Update all of the issues using this data. Try and only run a single query

data IssueData = IssueData
   { idKey = Maybe String
   , idSummary = Maybe String
   }

data UpdatedIssueData = UpdatedIssueData
   { uidIssueId :: AT.IssueId
   , uidNewData :: IssueData
   }

monadFind :: Monad m => (a -> f Bool) -> m a -> m (Maybe a)
monadFind isValue collection = 
   fmap (\x -> (x, isValue x)) collection
   
data WebhookData = WebhookData
   { wdIssue :: WebhookIssue
   , wdChangelog :: WebhookChangelog
   } deriving (Eq, Show, Generic)

data WebhookIssue = WebhookIssue
   { wiId :: AT.IssueId
   } deriving (Eq, Show, Generic)

data WebhookChangelog = WebhookChangelog
   { wcItems :: [ChangeLogItem]
   } deriving (Eq, Show, Generic)

data ChangeLogItem = ChangeLogItem
   { cliField :: String
   , cliFromString :: String
   , cliToString :: String
   } deriving (Eq, Show, Generic)

parseUpdatedKey :: Value -> Parser UpdatedIssueData
parseUpdatedKey json = withObject "webhook-json" parseChangelog json
   where
      parseWebhookData :: Object -> Parser UpdatedIssueData
      parseWebhookData j = do
         changelog <- j .: "changelog"
         withObject "changelog" parseChangelog changelog
      
      parseChangelog :: Object -> Parser IssueData
      parseChangelog changelogJson = do
         items <- changelogJson .: "items"
         withArray "items" parseItems items

      parseItems :: Array -> Parser IssueData
      parseItems items = do
         fmap (objectFieldIs "Key") items
         let potentialNewKey = findObject "field" "Key" items
         let potentialNewSummary = findObject "field" "Summary" items

-- type Array = Vector Value
-- type Object = HashMap Text Value
-- I have an array of objects and I want to find the first object in the array that has a field X

getValueAsString :: 

findObject :: Text -> Text -> Array -> Maybe Object
findObject key val items = find helper (objectsInArray items)
   where
      helper :: Object -> Bool
      helper o = fromMaybe False $ ((==) val <$> lookup key o)

objectsInArray :: Array -> [Object]
objectsInArray = V.foldr helper []
   where
      helper :: Value -> [Object] -> [Object]
      helper x@(Object _) xs = x : xs
      helper _ xs = xs
   
handleIssueDeleteWebhook :: AppHandler ()
handleIssueDeleteWebhook = handleMethods [(SC.POST, WC.tenantFromToken handleDelete)]

handleDelete :: CT.ConnectTenant -> AppHandler ()
handleDelete (tenant, _) = 
