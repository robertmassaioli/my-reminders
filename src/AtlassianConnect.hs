{-# LANGUAGE OverloadedStrings #-}

module AtlassianConnect
  ( addonDescriptor
  ) where

import Network.URI
import Data.Maybe
import Connect.Descriptor

atlassianHomepage :: URI
atlassianHomepage = fromJust $ parseURI "http://www.atlassian.com/"

addonDescriptor :: Plugin
addonDescriptor =
  basePlugin
    { pluginName      = Nothing -- Will be injected by the connect snaplet
    , pluginDescription  = Just "A universal personal reminder plugin for Cloud; never forget again."
    , vendor         = Just $ Vendor "Atlassian" atlassianHomepage
    , lifecycle = Just $ defaultLifecycle 
        { enabled = Nothing
        , disabled = Nothing
        }
    , modules = Just $ Modules emptyJiraModules
          { webPanels = 
            [ WebPanel
               { wpKey = "create-reminder-panel"
               , wpName = Name "My reminders"
               , wpUrl = "/panel/jira/reminder/create?issue_key={issue.key}&issue_id={issue.id}"
               , wpLocation = "atl.jira.view.issue.right.context"
               , wpConditions = [staticJiraCondition UserIsLoggedInJiraCondition]
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
          , webhooks = 
            [ Webhook { webhookEvent = JiraIssueUpdated, webhookUrl = "/rest/webhook/issue/update" }
            , Webhook { webhookEvent = JiraIssueDeleted, webhookUrl = "/rest/webhook/issue/delete" }
            ]
          }
    , scopes = Just [Read]
    , enableLicensing = Just False -- TODO Why is this a maybe type? What value does it add being potentially nothing?
    }
  where
    basePlugin = pluginDescriptor (PluginKey "com.atlassian.myreminders") nullURI jwtAuthentication
    jwtAuthentication = Authentication Jwt Nothing