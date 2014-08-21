{-# LANGUAGE DeriveGeneric #-}

module Connect.AtlassianTypes
   ( UserKey
   , UserEmail
   , IssueKey
   , IssueId
   , IssueSummary
   , UserDetails(..)
   , IssueDetails(..)
   ) where

import Mail.Hailgun
import GHC.Generics

type UserKey   = String    -- TODO does this need to be a Text type? What does String not support?
type UserEmail = UnverifiedEmailAddress
type IssueKey  = String    -- TODO does this need to be a Text type? What does String not support?
type IssueId   = Integer
type IssueSummary = String

data UserDetails = UserDetails
   { userKey :: UserKey
   , userEmail :: UserEmail
   } deriving (Show, Generic)

data IssueDetails = IssueDetails
   { issueKey :: IssueKey
   , issueId :: IssueId
   , issueSummary :: IssueSummary
   } deriving (Show, Generic)
