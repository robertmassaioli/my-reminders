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

import qualified Data.Text    as T
import           GHC.Generics
import           Mail.Hailgun

type UserKey   = T.Text
type UserEmail = UnverifiedEmailAddress
type IssueKey  = T.Text
type IssueId   = Integer
type IssueSummary = T.Text

data UserDetails = UserDetails
   { userKey   :: UserKey
   , userEmail :: UserEmail
   } deriving (Show, Generic)

data IssueDetails = IssueDetails
   { issueKey     :: IssueKey
   , issueId      :: IssueId
   , issueSummary :: IssueSummary
   } deriving (Show, Generic)
