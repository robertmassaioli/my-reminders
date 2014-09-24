{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

-- TODO be more selective about what we export from here
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

data Condition = SingleCondition
   { conditionSource    :: ConditionSource
   , conditionInverted  :: Bool
   -- , conditionParams    :: [(String, String)] -- TODO impliment properly but not required yet
   } 
   | CompositeCondition
   { subConditions :: [Condition]
   , conditionType :: ConditionType
   }
   deriving (Show)

staticJiraCondition :: JIRACondition -> Condition
staticJiraCondition c = SingleCondition { conditionSource = StaticJIRACondition c, conditionInverted = False }

staticConfluenceCondition :: ConfluenceCondition -> Condition
staticConfluenceCondition c = SingleCondition { conditionSource = StaticConfluenceCondition c, conditionInverted = False }

instance ToJSON Condition where
   toJSON sc@(SingleCondition {}) = object
      [ "condition" .= conditionSource sc
      , "invert" .= conditionInverted sc
      ]
   toJSON cc@(CompositeCondition {}) = object [ "conditions" .= subConditions cc, "type" .= conditionType cc]

data ConditionType = AndCondition | OrCondition
   deriving (Eq, Show)

instance ToJSON ConditionType where
   toJSON AndCondition = String "AND"
   toJSON OrCondition  = String "OR"

data ConditionSource 
   = StaticJIRACondition        JIRACondition
   | StaticConfluenceCondition  ConfluenceCondition
   | RemoteConnectCondition     RemoteCondition -- TODO merge the Remote Condition into this section
   deriving (Show, Eq)

instance ToJSON ConditionSource where
   toJSON (StaticJIRACondition x) = toJSON x
   toJSON (StaticConfluenceCondition x) = toJSON x
   toJSON (RemoteConnectCondition x) = toJSON x

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
   deriving (Eq, Show, Generic)

instance ToJSON JIRACondition where
   toJSON = toJSON . dropConditionAndSnakeCase . show

instance ToJSON ConfluenceCondition where
   toJSON = toJSON . dropConditionAndSnakeCase . show

dropConditionAndSnakeCase :: String -> String
dropConditionAndSnakeCase = camelToSnakeCase . dropCondition . lowerFirst

lowerFirst :: String -> String
lowerFirst (x : xs) = C.toLower x : xs
lowerFirst [] = []

camelToSnakeCase :: String -> String
camelToSnakeCase input = case L.break C.isUpper input of
   (first, []) -> first
   (first, x : xs) -> first ++ ('_' : C.toLower x : camelToSnakeCase xs)

dropCondition :: String -> String
dropCondition = L.reverse . try (L.stripPrefix reverseText) . L.reverse
   where
      reverseText :: String
      reverseText = L.reverse "Condition"

try :: (a -> Maybe a) -> a -> a
try f v = fromMaybe v (f v)

data ConfluenceCondition = ConfluenceCondition
   deriving (Eq, Show, Generic)

data RemoteCondition = RemoteCondition
   deriving (Eq, Show, Generic)


-- TODO give this more details
instance ToJSON RemoteCondition

data WebPanel = WebPanel
   { wpKey        :: Text
   , wpName       :: Name WebPanel
   , wpUrl        :: Text
   , wpLocation   :: Text
   , wpConditions :: [Condition]
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
