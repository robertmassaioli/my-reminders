{-# LANGUAGE DeriveGeneric #-}
module WebhookHandlers
   ( handleIssueUpdateWebhook
   , handleIssueDeleteWebhook
   ) where

import           AesonHelpers
import           Application
import           Control.Applicative        (pure, (<$>), (<*>))
import           Control.Monad              (void, when)
import           Data.Aeson
import           Data.Aeson.Types           (Options, defaultOptions,
                                             fieldLabelModifier)
import           Data.List                  as DL
import           Data.Maybe                 (isJust)
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple
import           GHC.Generics
import qualified Persistence.PostgreSQL     as DB
import qualified Persistence.Reminder       as P
import qualified Snap.AtlassianConnect      as AC
import qualified Snap.Core                  as SC
import qualified SnapHelpers                as SH
import qualified TenantJWT                  as WT

handleIssueUpdateWebhook :: AppHandler ()
handleIssueUpdateWebhook = SH.handleMethods [(SC.POST, WT.withTenant (handleWebhook handleUpdate))]

handleWebhook :: (AC.Tenant -> IssueUpdate -> AppHandler ()) -> AC.TenantWithUser -> AppHandler ()
handleWebhook webhookHandler (tenant, _) = do
   parsedRequest <- webhookDataFromRequest
   case parsedRequest of
      Left _ -> SH.respondNoContent
      Right webhookData -> do
         webhookHandler tenant (webhookDataToIssueUpdate webhookData)
         SH.respondNoContent

webhookDataFromRequest :: AppHandler (Either String WebhookData)
webhookDataFromRequest = eitherDecode <$> SC.readRequestBody dataLimitBytes
   where
      dataLimitBytes = 10 ^ (7 :: Integer) -- We want to not accept webhook responses larger than 10 MB

handleUpdate :: AC.Tenant -> IssueUpdate -> AppHandler ()
handleUpdate tenant issueUpdate = when (reminderUpdateRequired issueUpdate) $ DB.withConnection (handleWebhookUpdate tenant issueUpdate)

-- Issues will be updated many times and the connect webhooks cannot be easily restricted to certain
-- projects if we want to remain flexible. Therefore, for every instance that we are added to we are
-- going to recieve all of their issue update traffic. Therefore it is imperative that we stop
-- handling the webhook as soon as possible. Specifically, the original intention of this method was
-- to not open up a database connection unless we know, in advance, that it will be required.
-- Opening a database connection for every update request would otherwise be needlessly slow.
reminderUpdateRequired :: IssueUpdate -> Bool
reminderUpdateRequired iu = any isJust $ [iuNewKey, iuNewSummary] <*> pure iu

handleWebhookUpdate :: AC.Tenant -> IssueUpdate -> Connection -> IO ()
handleWebhookUpdate tenant issueUpdate conn = do
   maybe (return 0) (\newKey -> P.updateKeysForReminders tenant (iuId issueUpdate) newKey conn) (iuNewKey issueUpdate)
   maybe (return 0) (\newSummary -> P.updateSummariesForReminders tenant (iuId issueUpdate) newSummary conn) (iuNewSummary issueUpdate)
   return ()

webhookDataToIssueUpdate :: WebhookData -> IssueUpdate
webhookDataToIssueUpdate webhookData = IssueUpdate
   { iuId = read . wiId . wdIssue $ webhookData
   , iuNewKey = findCliToString (T.pack "Key") -- Key is uppercased to start
   , iuNewSummary = findCliToString (T.pack "summary") -- Summary is lower cased to start
   }
   where
      cli :: [ChangeLogItem]
      cli = maybe [] wcItems . wdChangelog $ webhookData

      findCliToString :: T.Text -> Maybe T.Text
      findCliToString = fmap cliToString . findCli

      findCli :: T.Text -> Maybe ChangeLogItem
      findCli fieldName = DL.find ((==) fieldName . cliField) cli

data IssueUpdate = IssueUpdate
   { iuId         :: AC.IssueId
   , iuNewKey     :: Maybe T.Text
   , iuNewSummary :: Maybe T.Text
   } deriving (Show)

parseOptions :: String -> Options
parseOptions prefix = defaultOptions { fieldLabelModifier = stripFieldNamePrefix prefix }

data WebhookData = WebhookData
   { wdIssue     :: WebhookIssue
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
   { cliField      :: T.Text
   , cliFromString :: T.Text
   , cliToString   :: T.Text
   } deriving (Eq, Show, Generic)

instance FromJSON ChangeLogItem where
   parseJSON = genericParseJSON (parseOptions "cli")

handleIssueDeleteWebhook :: AppHandler ()
handleIssueDeleteWebhook = SH.handleMethods [(SC.POST, WT.withTenant (handleWebhook handleDelete))]

-- Since deletes are will probably be infrequent just delete all reminders for the deleted issue
-- against that tenant in the database.
handleDelete :: AC.Tenant -> IssueUpdate -> AppHandler ()
handleDelete tenant issueUpdate = void $ DB.withConnection (P.deleteRemindersForIssue tenant (iuId issueUpdate))
