{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Restyler.GitHub.Api
  ( -- * Domain-specific constructs
    getPullRequest
  , getPullRequestFiles

    -- * "GitHub" dependency-injection
  , MonadGitHub (..)

    -- * @DerivingVia@
  , GitHubToken (..)
  , envGitHubToken
  , HasGitHubToken (..)
  , ActualGitHub (..)
  ) where

import Restyler.Prelude

import Data.Vector (Vector)
import Env qualified
import GitHub (github)
import GitHub qualified
import Restyler.GitHub.PullRequest
import Restyler.GitHub.PullRequest.File
import Restyler.GitHub.Repository

getPullRequest
  :: (MonadIO m, MonadGitHub m)
  => Repository
  -> Int
  -> m PullRequest
getPullRequest repo pr = do
  labels <-
    fromGitHubVector (Right . convertLabel)
      =<< ghLabelsOnIssue ghOwner ghRepo ghIssueId

  fromGitHub (Right . convertPullRequest labels)
    =<< ghPullRequest ghOwner ghRepo ghNumber
 where
  ghOwner = GitHub.mkOwnerName repo.owner
  ghRepo = GitHub.mkRepoName repo.repo
  ghNumber = GitHub.IssueNumber pr
  ghIssueId = GitHub.mkId Proxy pr

getPullRequestFiles
  :: (MonadIO m, MonadGitHub m)
  => Repository
  -> Int
  -> m [PullRequestFile]
getPullRequestFiles repo pr = do
  fromGitHubVector convertFile
    =<< ghPullRequestFiles ghOwner ghRepo ghNumber
 where
  ghOwner = GitHub.mkOwnerName repo.owner
  ghRepo = GitHub.mkRepoName repo.repo
  ghNumber = GitHub.IssueNumber pr

convertLabel :: GitHub.IssueLabel -> Label
convertLabel gh = Label {name = GitHub.untagName $ GitHub.labelName gh}

convertPullRequest
  :: [Label]
  -- ^ Separately converted 'Label's
  --
  -- The "GitHub" library doens't parse the @labels@ field of @PullRequest@, so
  -- we separately call @labelsOnIssueR@ for now and pass them in.
  -> GitHub.PullRequest
  -> PullRequest
convertPullRequest labels gh =
  PullRequest
    { number = GitHub.unIssueNumber $ GitHub.pullRequestNumber gh
    , title = GitHub.pullRequestTitle gh
    , user = convertUser $ GitHub.pullRequestUser gh
    , state = convertState $ GitHub.pullRequestState gh
    , labels = labels
    , head = convertCommit $ GitHub.pullRequestHead gh
    , base = convertCommit $ GitHub.pullRequestBase gh
    }

convertUser :: GitHub.SimpleUser -> User
convertUser gh = User {login = GitHub.untagName $ GitHub.simpleUserLogin gh}

convertState :: GitHub.IssueState -> PullRequestState
convertState = \case
  GitHub.StateOpen -> PullRequestOpen
  GitHub.StateClosed -> PullRequestClosed

convertCommit :: GitHub.PullRequestCommit -> Commit
convertCommit gh =
  Commit
    { ref = GitHub.pullRequestCommitRef gh
    , sha = GitHub.pullRequestCommitSha gh
    }

convertFile :: GitHub.File -> Either String PullRequestFile
convertFile gh = do
  status <- pullRequestFileStatusFromText $ GitHub.fileStatus gh
  pure PullRequestFile {filename = unpack $ GitHub.fileFilename gh, status}

fromGitHubVector
  :: (MonadIO m, Foldable t)
  => (a -> Either String b)
  -> Either GitHub.Error (t a)
  -> m [b]
fromGitHubVector f =
  either throwString pure <=< either throwIO (pure . traverse f . toList)

fromGitHub
  :: MonadIO m => (a -> Either String b) -> Either GitHub.Error a -> m b
fromGitHub f =
  either throwString pure <=< either throwIO (pure . f)

-- | A thin seam over "GitHub"
--
-- Each method should map to a @whateverR@ function and accept and return
-- arguments in "GitHub" types. We want the logic on top (combining and
-- modifying responses into our domain types) to be testable, and thus
-- implemented on top of this, not within.
class Monad m => MonadGitHub m where
  ghLabelsOnIssue
    :: GitHub.Name GitHub.Owner
    -> GitHub.Name GitHub.Repo
    -> GitHub.Id GitHub.Issue
    -> m (Either GitHub.Error (Vector GitHub.IssueLabel))

  ghPullRequest
    :: GitHub.Name GitHub.Owner
    -> GitHub.Name GitHub.Repo
    -> GitHub.IssueNumber
    -> m (Either GitHub.Error GitHub.PullRequest)

  ghPullRequestFiles
    :: GitHub.Name GitHub.Owner
    -> GitHub.Name GitHub.Repo
    -> GitHub.IssueNumber
    -> m (Either GitHub.Error (Vector GitHub.File))

newtype GitHubToken = GitHubToken
  { unwrap :: Text
  }
  deriving newtype (IsString)

envGitHubToken :: Env.Parser Env.Error GitHubToken
envGitHubToken =
  Env.var Env.str "GITHUB_TOKEN"
    $ Env.help "GitHub token with access to the repo and PR"

githubTokenGitHubAuth :: GitHubToken -> GitHub.Auth
githubTokenGitHubAuth = GitHub.OAuth . encodeUtf8 . (.unwrap)

class HasGitHubToken a where
  getGitHubToken :: a -> GitHubToken

instance HasGitHubToken GitHubToken where
  getGitHubToken = id

newtype ActualGitHub m a = ActualGitHub
  { unwrap :: m a
  }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadReader env
    , MonadIO
    )

instance (Monad m, MonadIO m, MonadReader env m, HasGitHubToken env) => MonadGitHub m where
  ghLabelsOnIssue owner repo number =
    runGitHub $ GitHub.labelsOnIssueR owner repo number GitHub.FetchAll

  ghPullRequest owner repo number =
    runGitHub $ GitHub.pullRequestR owner repo number

  ghPullRequestFiles owner repo number =
    runGitHub $ GitHub.pullRequestFilesR owner repo number GitHub.FetchAll

runGitHub
  :: ( MonadIO m
     , MonadReader env m
     , HasGitHubToken env
     , GitHub.GitHubRW req (IO b)
     )
  => req
  -> m b
runGitHub req = do
  auth <- asks $ githubTokenGitHubAuth . getGitHubToken
  liftIO $ github auth req
