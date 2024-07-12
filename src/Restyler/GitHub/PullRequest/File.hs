{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Restyler.GitHub.PullRequest.File
  ( PullRequestFile (..)
  , PullRequestFileStatus (..)
  , pullRequestFileStatusFromText
  , pullRequestFileStatusToText
  , pullRequestFileToChangedPath
  , getPullRequestFiles
  ) where

import Restyler.Prelude

import Data.Aeson
import Restyler.GitHub.Api

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
  deriving stock (Eq)

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

pullRequestFileToChangedPath :: PullRequestFile -> Maybe FilePath
pullRequestFileToChangedPath file = do
  guard
    $ file.status
    `elem` [ PullRequestFileAdded
           , PullRequestFileCopied
           , PullRequestFileChanged
           , PullRequestFileRenamed
           , PullRequestFileModified
           ]

  pure file.filename

getPullRequestFiles
  :: (MonadIO m, MonadReader env m, HasGitHubToken env)
  => Repository
  -> Int
  -> m [PullRequestFile]
getPullRequestFiles repo pr =
  getAll
    $ "https://api.github.com/repos/"
    <> unpack repo.owner
    <> "/"
    <> unpack repo.repo
    <> "/pulls/"
    <> show pr
    <> "/files"
