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
    , scopes = Just [Read, Write]
    , enableLicensing = Just False -- TODO Why is this a maybe type? What value does it add being potentially nothing?
    , apiMigrations = Just ApiMigrations
      { migrationGdpr = True
      , migrationSignedInstall = True
      }
    }
  where
    basePlugin = pluginDescriptor (PluginKey "com.atlassian.myreminders") nullURI jwtAuthentication
    jwtAuthentication = Authentication Jwt

addonJiraModules :: JIRAModules
addonJiraModules = emptyJIRAModules
    { jmWebPanels = Just
      [ WebPanel
         { wpKey = "view-issue-reminders"
         , wpName = simpleText "My reminders"
         , wpTooltip = Just . simpleText $ "Your reminders for this issue."
         , wpUrl = "/panel/jira/reminder/simple?issue_key={issue.key}&issue_id={issue.id}"
         , wpLocation = "atl.jira.view.issue.right.context"
         , wpConditions = [staticJiraCondition UserIsLoggedInJiraCondition]
         , wpWeight = Nothing
         , wpLayout = Nothing
         , wpParams = noParams
         }
      ]
    , jmGeneralPages = Just
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
      , JIRAPage
        { jiraPageName = simpleText "Create reminder"
        , jiraPageKey = "create-reminder-dialog"
        , jiraPageUrl = "/panel/v2/jira/reminder/create?issue_key={issue.key}&issue_id={issue.id}"
        , jiraPageLocation = Just "completely-invalid-location"
        , jiraPageIcon = Nothing
        , jiraPageWeight = Nothing
        , jiraPageConditions = [staticJiraCondition UserIsLoggedInJiraCondition]
        , jiraPageParams = noParams
        }
      ]
    , jmWebhooks = Just
      [ Webhook { webhookEvent = JiraIssueUpdated, webhookUrl = "/rest/webhook/issue/update" }
      , Webhook { webhookEvent = JiraIssueDeleted, webhookUrl = "/rest/webhook/issue/delete" }
      ]
    , jmJiraIssueGlances = Just
      [ JIRAIssueGlance
        { jigKey = "view-issue-glance-reminders"
        , jigName = simpleText "My Reminders"
        , jigContent = JIRAIssueGlanceContentLabel (simpleText "My Reminders")
        , jigIcon = IconDetails
            { iconUrl = "/static/frontend/logo.svg"
            , iconWidth = Just 24
            , iconHeight = Just 24
            }
        , jigTarget = JIRAIssueGlanceTargetWebPanel "/panel/jira/reminder/simple?issue_key={issue.key}&issue_id={issue.id}"
        , jigConditions = []
        }
      ]
    }
