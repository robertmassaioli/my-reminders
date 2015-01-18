{-# LANGUAGE OverloadedStrings #-}

module AtlassianConnect
  ( addonDescriptor
  ) where

import           Data.Connect.Descriptor
import           Data.Maybe
import           Network.URI

atlassianHomepage :: URI
atlassianHomepage = fromJust $ parseURI "http://www.atlassian.com/"

addonDescriptor :: Plugin
addonDescriptor =
  basePlugin
    { pluginName      = Just . Name $ "My Reminders"
    , pluginDescription  = Just "A universal personal reminder plugin for Cloud; never forget again."
    , vendor         = Just $ Vendor (Name "Atlassian") atlassianHomepage
    , lifecycle = Just $ defaultLifecycle
        { enabled = Nothing
        , disabled = Nothing
        }
    , modules = Just $ Modules addonJiraModules emptyConfluenceModules
    , scopes = Just [Read]
    , enableLicensing = Just False -- TODO Why is this a maybe type? What value does it add being potentially nothing?
    }
  where
    basePlugin = pluginDescriptor (PluginKey "com.atlassian.myreminders") nullURI jwtAuthentication
    jwtAuthentication = Authentication Jwt

addonJiraModules :: JIRAModules
addonJiraModules = emptyJIRAModules
    { jmWebPanels =
      [ WebPanel
         { wpKey = "create-reminder-panel"
         , wpName = simpleText "My reminders"
         , wpTooltip = Just $ simpleText "Your reminders for this issue."
         , wpUrl = "/panel/jira/reminder/create?issue_key={issue.key}&issue_id={issue.id}"
         , wpLocation = "atl.jira.view.issue.right.context"
         , wpConditions = [staticJiraCondition UserIsLoggedInJiraCondition]
         , wpWeight = Nothing
         , wpLayout = Nothing
         , wpParams = noParams
         }
      ]
    , jmGeneralPages =
      [ JIRAPage
         { jiraPageName = simpleText "My Reminders"
         , jiraPageKey = "view-my-reminders"
         , jiraPageUrl = "/panel/jira/reminders/view"
         -- See: https://developer.atlassian.com/display/JIRADEV/User+Accessible+Locations#UserAccessibleLocations-AddingNewItemstoExistingWebSections
         , jiraPageLocation = Just "system.user.options/personal"
         , jiraPageIcon = Nothing
         , jiraPageWeight = Nothing
         , jiraPageConditions = []
         , jiraPageParams = noParams
         }
      ]
    , jmWebhooks =
      [ Webhook { webhookEvent = JiraIssueUpdated, webhookUrl = "/rest/webhook/issue/update" }
      , Webhook { webhookEvent = JiraIssueDeleted, webhookUrl = "/rest/webhook/issue/delete" }
      ]
    }
