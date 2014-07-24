module Connect.AtlassianTypes where

import Mail.Hailgun

type UserKey   = String    -- TODO does this need to be a Text type? What does String not support?
type UserEmail = UnverifiedEmailAddress
type IssueKey  = String    -- TODO does this need to be a Text type? What does String not support?
type IssueId   = Integer
type IssueSubject = String
