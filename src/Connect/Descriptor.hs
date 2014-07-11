{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
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
   { pluginName :: Maybe Text
   , pluginDescription :: Maybe Text
   , pluginKey :: Text
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


data Vendor = Vendor {vendorName :: Text, vendorUrl :: URI} deriving (Show, Generic)

data Authentication = Authentication {authType :: AuthType, publicKey :: Maybe Text} deriving (Show, Generic)

data AuthType = OAuth | Jwt | None deriving (Show, Generic)

data Modules = Modules JiraModules deriving (Show, Generic) -- TODO

data ProductScope
  = Read
  | Write
  | Delete
  | ProjectAdmin   -- This is a JIRA only Scope (TODO can we serve the correct scope set to the correct plugins?)
  | SpaceAdmin    -- This is a Confluence only Scope (TODO can we serve the correct scope set to the correct plugins?)
  | Admin
  deriving (Show, Generic)

data JiraModules = JiraModules
   { webPanels :: [WebPanel]
   } deriving (Show, Generic)

data WebPanel = WebPanel
   { key :: Text
   , name :: NameValue
   , url :: Text
   , location :: Text
   } deriving (Show, Generic)

data NameValue = NameValue
   { value :: Text
   } deriving (Show, Generic)

data Lifecycle = Lifecycle
   { installed :: Maybe URI
   , uninstalled :: Maybe URI
   , enabled :: Maybe URI
   , disabled :: Maybe URI
   } deriving (Show, Generic) -- TODO

instance ToJSON Plugin where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "plugin"
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

instance ToJSON NameValue where
   toJSON = genericToJSON baseOptions

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

pluginDescriptor :: Text -> URI -> Authentication -> Plugin
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
