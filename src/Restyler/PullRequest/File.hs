module Restyler.PullRequest.File
  ( PullRequestFile (..)
  , PullRequestFileStatus (..)
  , pullRequestFileStatusFromText
  , pullRequestFileStatusToText
  ) where

import Restyler.Prelude

import Data.Aeson

data PullRequestFile = PullRequestFile
  { filename :: FilePath
  , status :: PullRequestFileStatus
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data PullRequestFileStatus
  = PullRequestFileAdded
  | PullRequestFileRemoved
  | PullRequestFileModified
  | PullRequestFileRenamed
  | PullRequestFileCopied
  | PullRequestFileChanged
  | PullRequestFileUnchanged

instance FromJSON PullRequestFileStatus where
  parseJSON = withText "status" $ either fail pure . pullRequestFileStatusFromText

instance ToJSON PullRequestFileStatus where
  toJSON = toJSON . pullRequestFileStatusToText
  toEncoding = toEncoding . pullRequestFileStatusToText

pullRequestFileStatusFromText :: Text -> Either String PullRequestFileStatus
pullRequestFileStatusFromText = \case
  "added" -> Right PullRequestFileAdded
  "removed" -> Right PullRequestFileRemoved
  "modified" -> Right PullRequestFileModified
  "renamed" -> Right PullRequestFileRenamed
  "copied" -> Right PullRequestFileCopied
  "changed" -> Right PullRequestFileChanged
  "unchanged" -> Right PullRequestFileUnchanged
  x -> Left $ "Invalid status " <> show x

pullRequestFileStatusToText :: PullRequestFileStatus -> Text
pullRequestFileStatusToText = \case
  PullRequestFileAdded -> "added"
  PullRequestFileRemoved -> "removed"
  PullRequestFileModified -> "modified"
  PullRequestFileRenamed -> "renamed"
  PullRequestFileCopied -> "copied"
  PullRequestFileChanged -> "changed"
  PullRequestFileUnchanged -> "unchanged"
