module Restyler.Clone
  ( CloneTimeoutError (..)
  , clonePullRequest
  ) where

import Restyler.Prelude

import Restyler.AnnotatedException (throw)
import Restyler.Git (MonadGit (..))
import Restyler.GitHub.Api
import Restyler.Options.PullRequest
import Restyler.Options.Repository
import Restyler.Statsd (HasStatsClient)
import Restyler.Statsd qualified as Statsd

clonePullRequest
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadGit m
     , MonadReader env m
     , HasStatsClient env
     , HasGitHubToken env
     )
  => PullRequestOption
  -> m ()
clonePullRequest pr = do
  logInfo "Cloning repository"
  token <- asks $ (.unwrap) . getGitHubToken
  wrapClone $ do
    gitInit
    gitRemoteAdd "origin"
      $ unpack
      $ "https://x-access-token:"
      <> token
      <> "@github.com/"
      <> pr.repo.owner
      <> "/"
      <> pr.repo.repo
      <> ".git"

    let n = show pr.number
    gitFetch "origin" $ "pull/" <> n <> "/head:pull-" <> n
    gitSwitch $ "pull-" <> n

newtype CloneTimeoutError = CloneTimeoutError
  { unwrap :: Int
  }
  deriving stock (Show)

instance Exception CloneTimeoutError where
  displayException ex =
    "Clone timed out after "
      <> show @String ex.unwrap
      <> " minutes"

wrapClone
  :: (MonadUnliftIO m, MonadReader env m, HasStatsClient env) => m a -> m ()
wrapClone f = do
  mResult <- Statsd.wrap "restyler.clone" [] (30 * 60) f
  when (isNothing mResult) $ throw timedOutError
 where
  timedOutError = CloneTimeoutError 30
