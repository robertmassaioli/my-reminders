{-# LANGUAGE DeriveGeneric             #-}
module WebhookHandlers 
   ( handleIssueUpdateWebhook
   , handleIssueDeleteWebhook
   ) where

import           AesonHelpers
import           Application
import           Control.Monad            (when)
import qualified Connect.AtlassianTypes   as AT
import qualified Connect.Tenant           as CT
import           Data.Aeson
import           Data.Aeson.Types         (defaultOptions, fieldLabelModifier, Options)
import           Data.List                as DL
import           Data.Maybe               (fromMaybe, isJust)
import           Database.PostgreSQL.Simple
import           GHC.Generics
import qualified Persistence.Reminder     as P
import qualified Persistence.PostgreSQL   as DB
import qualified Persistence.Tenant       as PT
import qualified Snap.Core                as SC
import qualified SnapHelpers              as SH
import qualified TenantJWT                as WT

handleIssueUpdateWebhook :: AppHandler ()
handleIssueUpdateWebhook = SH.handleMethods [(SC.POST, WT.withTenant (handleWebhook handleUpdate))]

handleWebhook :: (PT.Tenant -> IssueUpdate -> AppHandler ()) -> CT.ConnectTenant -> AppHandler ()
handleWebhook webhookHandler (tenant, _) = do
   parsedRequest <- webhookDataFromRequest
   case parsedRequest of
      Left _ -> SH.respondNoContent 
      Right webhookData -> do
         webhookHandler tenant (webhookDataToIssueUpdate webhookData)
         SH.respondNoContent

webhookDataFromRequest :: AppHandler (Either String WebhookData)
webhookDataFromRequest = fmap eitherDecode $ SC.readRequestBody (1024 * 10)

handleUpdate :: PT.Tenant -> IssueUpdate -> AppHandler ()
handleUpdate tenant issueUpdate = when (reminderUpdateRequired issueUpdate) $ DB.withConnection (handleWebhookUpdate tenant issueUpdate)

-- Issues will be updated many times and the connect webhooks cannot be easily restricted to certain
-- projects if we want to remain flexible. Therefore, for every instance that we are added to we are
-- going to recieve all of their issue update traffic. Therefore it is imperative that we stop
-- handling the webhook as soon as possible. Specifically, the original intention of this method was
-- to not open up a database connection unless we know, in advance, that it will be required.
-- Opening a database connection for every update request would otherwise be needlessly slow.
reminderUpdateRequired :: IssueUpdate -> Bool
reminderUpdateRequired iu = any isJust . fmap ($ iu) $ [iuNewKey, iuNewSummary]

handleWebhookUpdate :: PT.Tenant -> IssueUpdate -> Connection -> IO ()
handleWebhookUpdate tenant issueUpdate conn = do
   maybe (return 0) (\newKey -> P.updateKeysForReminders tenant (iuId issueUpdate) newKey conn) (iuNewKey issueUpdate)
   maybe (return 0) (\newSummary -> P.updateSummariesForReminders tenant (iuId issueUpdate) newSummary conn) (iuNewSummary issueUpdate)
   return ()

webhookDataToIssueUpdate :: WebhookData -> IssueUpdate
webhookDataToIssueUpdate webhookData = IssueUpdate
   { iuId = read . wiId . wdIssue $ webhookData
   , iuNewKey = findCliToString "Key" -- Key is uppercased to start
   , iuNewSummary = findCliToString "summary" -- Summary is lower cased to start
   }
   where
      cli :: [ChangeLogItem]
      cli = fromMaybe [] . fmap wcItems . wdChangelog $ webhookData

      findCliToString :: String -> Maybe String
      findCliToString = fmap cliToString . findCli

      findCli :: String -> Maybe ChangeLogItem
      findCli fieldName = DL.find (((==) fieldName) . cliField) cli

data IssueUpdate = IssueUpdate
   { iuId         :: AT.IssueId
   , iuNewKey     :: Maybe String
   , iuNewSummary :: Maybe String
   } deriving (Show)

parseOptions :: String -> Options
parseOptions prefix = defaultOptions { fieldLabelModifier = stripFieldNamePrefix prefix }
   
data WebhookData = WebhookData
   { wdIssue :: WebhookIssue
   , wdChangelog :: Maybe WebhookChangelog
   } deriving (Eq, Show, Generic)

instance FromJSON WebhookData where
   parseJSON = genericParseJSON (parseOptions "wd")

data WebhookIssue = WebhookIssue
   { wiId :: String -- This is because it gets passed as a string
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

handleIssueDeleteWebhook :: AppHandler ()
handleIssueDeleteWebhook = SH.handleMethods [(SC.POST, WT.withTenant (handleWebhook handleDelete))]

-- Since deletes are will probably be infrequent just delete all reminders for the deleted issue
-- against that tenant in the database.
handleDelete :: PT.Tenant -> IssueUpdate -> AppHandler ()
handleDelete tenant issueUpdate = DB.withConnection (P.deleteRemindersForIssue tenant (iuId issueUpdate)) >> return ()
