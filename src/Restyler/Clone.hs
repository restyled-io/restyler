module Restyler.Clone
  ( CloneTimeoutError (..)
  , clonePullRequest
  ) where

import Restyler.Prelude

import Restyler.App.Class (MonadProcess, MonadSystem)
import Restyler.Git (gitCloneBranchByRef)
import Restyler.GitHub.Api
import Restyler.Options.PullRequest
import Restyler.Options.Repository
import Restyler.Statsd (HasStatsClient)
import Restyler.Statsd qualified as Statsd

clonePullRequest
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadSystem m
     , MonadProcess m
     , MonadReader env m
     , HasStatsClient env
     , HasGitHubToken env
     )
  => PullRequestOption
  -> m ()
clonePullRequest pr = do
  logInfo "Cloning repository"
  token <- asks $ (.unwrap) . getGitHubToken
  wrapClone
    $ gitCloneBranchByRef
      ("pull/" <> show pr.number <> "/head")
      ("pull-" <> show pr.number)
      ( unpack
          $ "https://x-access-token:"
          <> token
          <> "@github.com/"
          <> pr.repo.owner
          <> "/"
          <> pr.repo.repo
          <> ".git"
      )
      "."

newtype CloneTimeoutError = CloneTimeoutError
  { cloneTimeoutDurationMinutes :: Int
  }
  deriving stock (Show)

instance Exception CloneTimeoutError where
  displayException ex =
    "Clone timed out after "
      <> show @String (cloneTimeoutDurationMinutes ex)
      <> " minutes"

wrapClone
  :: (MonadUnliftIO m, MonadReader env m, HasStatsClient env) => m a -> m ()
wrapClone f = do
  mResult <- Statsd.wrap "restyler.clone" [] (30 * 60) f
  when (isNothing mResult) $ throwIO timedOutError
 where
  timedOutError = CloneTimeoutError 30
