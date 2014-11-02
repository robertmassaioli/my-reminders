{-# LANGUAGE DeriveGeneric             #-}
module WebhookHandlers 
   ( handleIssueUpdateWebhook
   , handleIssueDeleteWebhook
   ) where

import           AesonHelpers
import           Application
import           Control.Applicative      ((<$>))
import           Control.Monad.IO.Class   (liftIO)
import qualified Connect.AtlassianTypes   as AT
import qualified Connect.Tenant           as CT
import           Data.Aeson
import           Data.Aeson.Types         (defaultOptions, fieldLabelModifier, Options)
import           Data.Maybe (fromMaybe)
import           Data.Text                as T
import           GHC.Generics
import qualified Snap.Core                as SC
import qualified SnapHelpers              as SH
import qualified TenantJWT                as WT

handleIssueUpdateWebhook :: AppHandler ()
handleIssueUpdateWebhook = SH.handleMethods [(SC.POST, (liftIO . print $ "passing onwards") >>  WT.withTenant handleUpdate)]

handleUpdate :: CT.ConnectTenant -> AppHandler ()
handleUpdate (tenant, _) = do
   liftIO . print $ "Handling the webhook update request"
   -- Parse the Webhook JSON message
   request <- SC.readRequestBody (1024 * 10) -- TODO this magic number is crappy, improve
   let parsedRequest = eitherDecode request :: (Either String WebhookData)
   liftIO . print $ parsedRequest
   case parsedRequest of
      Left parseError -> SH.respondNoContent 
      Right webhookData -> liftIO . print $ webhookData
         -- Update all of the issues using this data. Try and only run a single query

data IssueData = IssueData
   { idKey :: Maybe String
   , idSummary :: Maybe String
   }

data UpdatedIssueData = UpdatedIssueData
   { uidIssueId :: AT.IssueId
   , uidNewData :: IssueData
   }

{-
monadFind :: Monad m => (a -> f Bool) -> m a -> m (Maybe a)
monadFind isValue collection = 
   fmap (\x -> (x, isValue x)) collection
-}

parseOptions :: String -> Options
parseOptions prefix = defaultOptions { fieldLabelModifier = stripFieldNamePrefix prefix }
   
data WebhookData = WebhookData
   { wdIssue :: WebhookIssue
   , wdChangelog :: WebhookChangelog
   } deriving (Eq, Show, Generic)

instance FromJSON WebhookData where
   parseJSON = genericParseJSON (parseOptions "wd")

data WebhookIssue = WebhookIssue
   { wiId :: AT.IssueId
   } deriving (Eq, Show, Generic)

instance FromJSON WebhookIssue where
   parseJSON = genericParseJSON (parseOptions "wi")

data WebhookChangelog = WebhookChangelog
   { wcItems :: [ChangeLogItem]
   } deriving (Eq, Show, Generic)

instance FromJSON WebhookChangelog where
   parseJSON = genericParseJSON (parseOptions "wc")

data ChangeLogItem = ChangeLogItem
   { cliField :: String
   , cliFromString :: String
   , cliToString :: String
   } deriving (Eq, Show, Generic)

instance FromJSON ChangeLogItem where
   parseJSON = genericParseJSON (parseOptions "cli")

{-
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
-}

-- type Array = Vector Value
-- type Object = HashMap Text Value
-- I have an array of objects and I want to find the first object in the array that has a field X

--getValueAsString :: 

{-
findObject :: T.Text -> T.Text -> Array -> Maybe Object
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
-}
   
handleIssueDeleteWebhook :: AppHandler ()
handleIssueDeleteWebhook = SH.handleMethods [(SC.POST, WT.withTenant handleDelete)]

handleDelete :: CT.ConnectTenant -> AppHandler ()
handleDelete (tenant, _) =  undefined
