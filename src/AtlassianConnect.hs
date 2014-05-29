{-# LANGUAGE OverloadedStrings #-}

module AtlassianConnect (
    addonDescriptor
  , publicKeyUri
) where

import qualified Data.ByteString.Char8 as B
import           Network.URI
import           Data.Maybe
import           Connect.Descriptor

import           Data.Text

-- TODO Don't accept bytestring if you really wanted a URI, just expect the URI and push
-- the logic upwards
addonDescriptor :: B.ByteString -> Plugin
addonDescriptor baseUrl = 
   basePlugin
      { pluginName         = Just "PingMe for OnDemand"
      , pluginDescription  = Just "A universal PingMe plugin for OnDemand; never forget again."
      , vendor             = Just $ Vendor "Atlassian" (absoluteURI "http://www.atlassian.com/")
      , lifecycle = Just $ Lifecycle
            { installed = uri "/installed"
            , uninstalled = Nothing
            , enabled = Nothing
            , disabled = Nothing
            }
      , modules = Just $ 
         Modules JiraModules
            { webPanels = [ WebPanel 
                { key = "ping-create-panel"
                , name = NameValue "Create pings"
                , url = "/panel/ping/create?issue_key={issue.key}&issue_id={issue.id}"
                , location = "atl.jira.view.issue.right.context" 
                }
              ]
            }
      , scopes = Just ["READ"] -- TODO Stringly typed
      , enableLicensing = Just False -- TODO Why is this a maybe type? What value does it add being potentially nothing?
      }
   where
      basePlugin = pluginDescriptor connectPluginKey baseURI jwtAuthentication

      connectPluginKey :: Text -- Stringly typed...
      connectPluginKey = "com.atlassian.pingme" -- TODO should be configurable

      baseURI :: URI
      baseURI = absoluteURI $ B.unpack baseUrl

      jwtAuthentication = Authentication Jwt Nothing

publicKeyUri :: URI
publicKeyUri = fromMaybe nullURI $ uri "/plugins/servlet/oauth/consumer-info"

uri :: String -> Maybe URI
uri = parseRelativeReference

absoluteURI :: String -> URI
absoluteURI = fromMaybe nullURI . parseURI
