{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Connect.Descriptor where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Char as C
import Data.Text
import Data.Maybe
import qualified Data.List as L
import GHC.Generics
import Network.URI

data Plugin = Plugin
   { pluginName :: Maybe (Name Plugin)
   , pluginDescription :: Maybe Text
   , pluginKey :: PluginKey
   , pluginBaseUrl :: URI
   , vendor :: Maybe Vendor
   , authentication :: Authentication
   , apiVersion :: Maybe Text
   , modules :: Maybe Modules
   , enableLicensing :: Maybe Bool
   , lifecycle :: Maybe Lifecycle
   , links :: Maybe [(Text, URI)]
   , scopes :: Maybe [ProductScope]
   } deriving (Show, Generic)

data Name a = Name Text deriving (Show, Eq, Generic)

data PluginKey = PluginKey Text deriving (Show, Eq, Generic)

newtype Timeout = Timeout Integer deriving (Show, Eq, Enum, Num, Ord, Real, Integral)

data Vendor = Vendor {vendorName :: Text, vendorUrl :: URI} deriving (Show, Eq, Generic)

data Authentication = Authentication {authType :: AuthType, publicKey :: Maybe Text} deriving (Show, Eq, Generic)

data AuthType = OAuth | Jwt | None deriving (Show, Eq, Generic)

data Modules = Modules JiraModules deriving (Show, Generic) -- TODO

data ProductScope
  = Read
  | Write
  | Delete
  | ProjectAdmin   -- This is a JIRA only Scope (TODO can we serve the correct scope set to the correct plugins?)
  | SpaceAdmin    -- This is a Confluence only Scope (TODO can we serve the correct scope set to the correct plugins?)
  | Admin
  deriving (Show, Eq, Generic)

data JiraModules = JiraModules
   { webPanels :: [WebPanel]
   , generalPages :: [GeneralPage]
   } deriving (Show, Generic)

emptyJiraModules :: JiraModules
emptyJiraModules = JiraModules [] []

data Condition = Condition
   { conditionSource    :: ConditionSource
   , conditionInverted  :: Bool
   -- , conditionParams    :: [(String, String)] -- TODO impliment properly but not required yet
   } 

data ConditionSource 
   = ConditionByJIRA        JIRACondition
   = ConditionByConfluence  ConfluenceCondition
   = ConditionByRemote      RemoteCondition

-- The JIRA Conditions have been taken from:
-- https://developer.atlassian.com/static/connect/docs/modules/fragment/single-condition.html
-- as of the following date: Tue 23 Sep 2014 08:45:49 EST
-- Please update the date above whenever you update these conditions.
data JIRACondition
   = CanAttachFileToIssueCondition
   | CanManageAttachmentsCondition
   | FeatureFlagCondition
   | HasIssuePermissionCondition
   | HasProjectPermissionCondition
   | HasSelectedProjectPermissionCondition
   | HasSubTasksAvaliableCondition
   | HasVotedForIssueCondition
   | IsAdminModeCondition
   | IsIssueAssignedToCurrentUserCondition
   | IsIssueEditableCondition
   | IsIssueReportedByCurrentUserCondition
   | IsIssueUnresolvedCondition
   | IsSubTaskCondition
   | IsWatchingIssueCondition
   | LinkingEnabledCondition
   | SubTasksEnabledCondition
   | TimeTrackingEnabledCondition
   | UserHasIssueHistoryCondition
   | UserIsAdminCondition
   | UserIsLoggedInCondition
   | UserIsProjectAdminCondition
   | UserIsSysadminCondition
   | UserIsTheLoggedInUserCondition
   | VotingEnabledCondition
   | WatchingEnabledCondition
   deriving (Eq, Show)

data ConfluenceCondition

data RemoteCondition = RemoteCondition
   { 

data WebPanel = WebPanel
   { wpKey :: Text
   , wpName :: Name WebPanel
   , wpUrl :: Text
   , wpLocation :: Text
   } deriving (Show, Generic)

data GeneralPage = GeneralPage
   { generalPageUrl :: Text
   , generalPageName :: Name GeneralPage
   , generalPageKey :: Text
   , generalPageLocation :: Maybe Text
   , generalPageIcon :: Maybe IconDetails
   , generalPageWeight :: Maybe Integer
   } deriving (Show, Generic)

data IconDetails = IconDetails
   { iconUrl :: Text
   , iconWidth :: Maybe Integer
   , iconHeight :: Maybe Integer
   } deriving (Show, Generic)

data Lifecycle = Lifecycle
   { installed :: Maybe URI
   , uninstalled :: Maybe URI
   , enabled :: Maybe URI
   , disabled :: Maybe URI
   } deriving (Show, Generic) -- TODO

instance ToJSON PluginKey

instance ToJSON (Name Plugin)
instance ToJSON (Name PluginKey)

instance ToJSON (Name WebPanel) where
   toJSON = nameToValue

instance ToJSON (Name GeneralPage) where
   toJSON = nameToValue

nameToValue :: Name a -> Value
nameToValue (Name name) = object [ "value" .= name ]

instance ToJSON Plugin where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "plugin"
      }

instance ToJSON GeneralPage where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "generalPage"
      }

instance ToJSON IconDetails where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "icon"
      }

instance ToJSON Vendor where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "vendor"
      }

instance ToJSON Authentication where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "auth"
      }

instance ToJSON AuthType where
   toJSON OAuth = "oauth"
   toJSON Jwt  = "jwt"
   toJSON None  = "none"

instance ToJSON URI where
   toJSON = String . pack . show

instance ToJSON Lifecycle where
   toJSON = genericToJSON baseOptions

instance ToJSON Modules where
   toJSON = genericToJSON baseOptions

instance ToJSON ProductScope where
   toJSON Read        = "read"
   toJSON Write        = "write"
   toJSON Delete       = "delete"
   toJSON ProjectAdmin   = "project_admin"
   toJSON SpaceAdmin    = "space_admin"
   toJSON Admin        = "admin"

instance FromJSON ProductScope where
  parseJSON (String "read")        = return Read
  parseJSON (String "write")       = return Write
  parseJSON (String "delete")      = return Delete
  parseJSON (String "project_admin")  = return ProjectAdmin
  parseJSON (String "space_admin")   = return SpaceAdmin
  parseJSON (String "admin")       = return Admin
  parseJSON _                 = mzero

instance ToJSON JiraModules where
   toJSON = genericToJSON baseOptions

instance ToJSON WebPanel where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "wp"
      }

instance FromJSON URI where
   parseJSON (String uriString) = maybe mzero return (parseURI $ unpack uriString)
   parseJSON _ = mzero

baseOptions :: Options
baseOptions = defaultOptions
   { omitNothingFields = True
   }

stripFieldNamePrefix :: String -> String -> String
stripFieldNamePrefix pre s = toLowerFirst $ fromMaybe s (L.stripPrefix pre s)
   where toLowerFirst (c : cs) = C.toLower c : cs
         toLowerFirst [] = []

defaultLifecycle :: Lifecycle
defaultLifecycle = Lifecycle
   { installed = parseURI "/installed"
   , uninstalled = parseURI "/uninstalled"
   , enabled = parseURI "/enabled"
   , disabled = parseURI "/disabled"
   }

pluginDescriptor :: PluginKey -> URI -> Authentication -> Plugin
pluginDescriptor key' url' auth = Plugin
   { pluginName = Nothing
   , pluginDescription = Nothing
   , pluginKey = key'
   , pluginBaseUrl = url'
   , vendor = Nothing
   , authentication = auth
   , apiVersion = Nothing
   , modules = Nothing
   , enableLicensing = Nothing
   , lifecycle = Nothing
   , links = Nothing
   , scopes = Nothing
   }
