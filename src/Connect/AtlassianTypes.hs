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

import qualified Data.ByteString as B
import qualified Data.Text       as T
import           GHC.Generics

type UserKey   = T.Text
type UserEmail = B.ByteString    -- TODO use a standard email type
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
