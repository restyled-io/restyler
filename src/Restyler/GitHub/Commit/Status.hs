module Restyler.GitHub.Commit.Status
  ( CommitStatusState (..)
  ) where

data CommitStatusState
  = CommitStatusPending
  | CommitStatusSuccess
  | CommitStatusError
  | CommitStatusFailure
