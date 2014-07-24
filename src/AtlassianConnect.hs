{-# LANGUAGE OverloadedStrings #-}

module AtlassianConnect
  ( addonDescriptor
  , publicKeyUri
  , DescriptorConfig(..)
  ) where

import Network.URI
import Data.Maybe
import Connect.Descriptor
import Connect.Data

data DescriptorConfig = DescriptorConfig
  { dcPluginName :: Name Connect
  , dcPluginKey :: PluginKey
  , dcBaseUrl :: URI
  }

atlassianHomepage :: URI
atlassianHomepage = fromJust $ parseURI "http://www.atlassian.com/"

addonDescriptor :: DescriptorConfig -> Plugin
addonDescriptor descriptorConfig =
  basePlugin
    { pluginName      = Just $ case (dcPluginName descriptorConfig) of Name t -> Name t
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
    basePlugin = pluginDescriptor (dcPluginKey descriptorConfig) baseURI jwtAuthentication
    baseURI = dcBaseUrl descriptorConfig
    jwtAuthentication = Authentication Jwt Nothing

publicKeyUri :: URI
publicKeyUri = fromMaybe nullURI $ parseRelativeReference "/plugins/servlet/oauth/consumer-info"
