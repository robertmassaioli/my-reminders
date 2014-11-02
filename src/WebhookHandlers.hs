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
import           Database.PostgreSQL.Simple
import           GHC.Generics
import qualified Persistence.Ping         as P
import qualified Persistence.PostgreSQL   as DB
import qualified Persistence.Tenant       as PT
import qualified Snap.Core                as SC
import qualified SnapHelpers              as SH
import qualified TenantJWT                as WT

handleIssueUpdateWebhook :: AppHandler ()
handleIssueUpdateWebhook = SH.handleMethods [(SC.POST, WT.withTenant handleUpdate)]

handleUpdate :: CT.ConnectTenant -> AppHandler ()
handleUpdate (tenant, _) = do
   request <- SC.readRequestBody (1024 * 10) -- TODO this magic number is crappy, improve
   let parsedRequest = eitherDecode request :: (Either String WebhookData)
   case parsedRequest of
      Left _ -> SH.respondNoContent 
      Right webhookData -> do
         DB.withConnection $ \conn -> handleWebhookUpdate conn tenant . webhookDataToIssueUpdate $ webhookData
         SH.respondNoContent

handleWebhookUpdate :: Connection -> PT.Tenant -> IssueUpdate -> IO ()
handleWebhookUpdate conn tenant issueUpdate = do
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
      cli = wcItems . wdChangelog $ webhookData

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
   , wdChangelog :: WebhookChangelog
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
handleIssueDeleteWebhook = SH.handleMethods [(SC.POST, WT.withTenant handleDelete)]

handleDelete :: CT.ConnectTenant -> AppHandler ()
handleDelete (tenant, _) =  undefined
