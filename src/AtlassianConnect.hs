{-# LANGUAGE OverloadedStrings #-}

module AtlassianConnect
  ( addonDescriptor
  , publicKeyUri
  , DynamicDescriptorConfig(..)
  ) where

import Network.URI
import Data.Maybe
import Connect.Descriptor

data DynamicDescriptorConfig = DynamicDescriptorConfig
  { dcPluginName :: Name Plugin
  , dcPluginKey :: PluginKey
  , dcBaseUrl :: URI
  }

atlassianHomepage :: URI
atlassianHomepage = fromJust $ parseURI "http://www.atlassian.com/"

addonDescriptor :: DynamicDescriptorConfig -> Plugin
addonDescriptor descriptorConfig =
  basePlugin
    { pluginName      = Just $ dcPluginName descriptorConfig
    , pluginDescription  = Just "A universal PingMe plugin for OnDemand; never forget again."
    , vendor         = Just $ Vendor "Atlassian" atlassianHomepage
    , lifecycle = Just Lifecycle
        { installed = parseRelativeReference "/installed"
        , uninstalled = Nothing
        , enabled = Nothing
        , disabled = Nothing
        }
    , modules = Just $ Modules JiraModules
          { webPanels = 
            [ WebPanel
               { wpKey = "ping-create-panel"
               , wpName = Name "My reminders"
               , wpUrl = "/panel/jira/ping/create?issue_key={issue.key}&issue_id={issue.id}"
               , wpLocation = "atl.jira.view.issue.right.context"
               , wpConditions = [staticJiraCondition UserIsLoggedInCondition]
               }
            ]
          , generalPages = 
            [ GeneralPage
               { generalPageName = Name "My Reminders"
               , generalPageKey = "view-my-reminders"
               , generalPageUrl = "/panel/jira/reminders/view"
               -- See: https://developer.atlassian.com/display/JIRADEV/User+Accessible+Locations#UserAccessibleLocations-AddingNewItemstoExistingWebSections
               , generalPageLocation = Just "system.user.options/personal"
               , generalPageIcon = Nothing
               , generalPageWeight = Nothing
               }
            ]
          }
    , scopes = Just [Read]
    , enableLicensing = Just False -- TODO Why is this a maybe type? What value does it add being potentially nothing?
    }
  where
    basePlugin = pluginDescriptor (dcPluginKey descriptorConfig) baseURI jwtAuthentication
    baseURI = dcBaseUrl descriptorConfig
    jwtAuthentication = Authentication Jwt Nothing

publicKeyUri :: URI
publicKeyUri = fromMaybe nullURI $ parseRelativeReference "/plugins/servlet/oauth/consumer-info"
