{-# LANGUAGE DeriveGeneric             #-}
module WebhookHandlers 
   ( handleIssueUpdateWebhook
   , handleIssueDeleteWebhook
   ) where

import           AesonHelpers
import           Application
import           Control.Monad.IO.Class   (liftIO)
import qualified Connect.AtlassianTypes   as AT
import qualified Connect.Tenant           as CT
import           Data.Aeson
import           Data.Aeson.Types         (defaultOptions, fieldLabelModifier, Options)
import           Data.List                as DL
import           Data.Maybe               (fromMaybe)
import           Database.PostgreSQL.Simple
import           GHC.Generics
import qualified Persistence.Ping         as P
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

handleUpdate :: PT.Tenant -> IssueUpdate -> AppHandler ()
handleUpdate tenant issueUpdate = DB.withConnection (handleWebhookUpdate tenant issueUpdate)

webhookDataFromRequest :: AppHandler (Either String WebhookData)
webhookDataFromRequest = fmap eitherDecode $ SC.readRequestBody (1024 * 10)

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

handleDelete :: PT.Tenant -> IssueUpdate -> AppHandler ()
handleDelete tenant issueUpdate = DB.withConnection (P.deleteRemindersForIssue tenant (iuId issueUpdate)) >> return ()
