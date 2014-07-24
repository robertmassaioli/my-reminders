{-# LANGUAGE OverloadedStrings #-}

module AtlassianConnect
  ( addonDescriptor
  , publicKeyUri
  , DescriptorConfig(..)
  ) where

import Network.URI
import Data.Maybe
import Connect.Descriptor

import Data.Text

import qualified Data.ByteString.Char8 as B

data DescriptorConfig = DescriptorConfig
  { dcPluginName :: Text
  , dcPluginKey :: Text
  , dcBaseUrl :: B.ByteString
  }

atlassianHomepage :: URI
atlassianHomepage = fromJust $ parseURI "http://www.atlassian.com/"

-- TODO Don't accept bytestring if you really wanted a URI, just expect the URI and push
-- the logic upwards
addonDescriptor :: DescriptorConfig -> Plugin
addonDescriptor descriptorConfig =
  basePlugin
    { pluginName      = Just . dcPluginName $ descriptorConfig
    , pluginDescription  = Just "A universal PingMe plugin for OnDemand; never forget again."
    , vendor         = Just $ Vendor "Atlassian" atlassianHomepage
    , lifecycle = Just Lifecycle
        { installed = parseRelativeReference "/installed"
        , uninstalled = Nothing
        , enabled = Nothing
        , disabled = Nothing
        }
    , modules = Just $ Modules JiraModules
          { webPanels = [ WebPanel
              { key = "ping-create-panel"
              , name = NameValue "My reminders"
              , url = "/panel/ping/create?issue_key={issue.key}&issue_id={issue.id}"
              , location = "atl.jira.view.issue.right.context"
              }
            ]
          }
    , scopes = Just [Read] -- TODO Stringly typed
    , enableLicensing = Just False -- TODO Why is this a maybe type? What value does it add being potentially nothing?
    }
  where
    basePlugin = pluginDescriptor connectPluginKey baseURI jwtAuthentication

    connectPluginKey :: Text
    connectPluginKey = dcPluginKey descriptorConfig

    baseURI :: URI
    baseURI = fromMaybe nullURI . parseURI . B.unpack . dcBaseUrl $ descriptorConfig

    jwtAuthentication = Authentication Jwt Nothing

publicKeyUri :: URI
publicKeyUri = fromMaybe nullURI $ parseRelativeReference "/plugins/servlet/oauth/consumer-info"
