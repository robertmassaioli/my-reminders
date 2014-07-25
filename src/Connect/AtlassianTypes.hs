{-# LANGUAGE DeriveGeneric #-}

module Connect.AtlassianTypes
   ( UserKey
   , UserEmail
   , IssueKey
   , IssueId
   , IssueSubject
   , UserDetails(..)
   , IssueDetails(..)
   ) where

import Mail.Hailgun
import GHC.Generics

type UserKey   = String    -- TODO does this need to be a Text type? What does String not support?
type UserEmail = UnverifiedEmailAddress
type IssueKey  = String    -- TODO does this need to be a Text type? What does String not support?
type IssueId   = Integer
type IssueSubject = String

data UserDetails = UserDetails
   { userKey :: UserKey
   , userEmail :: UserEmail
   } deriving (Show, Generic)

data IssueDetails = IssueDetails
   { issueKey :: IssueKey
   , issueId :: IssueId
   , issueSubject :: IssueSubject
   } deriving (Show, Generic)
