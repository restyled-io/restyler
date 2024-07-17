module Restyler.Setup
  ( restylerSetup

    -- * Errors
  , PlanUpgradeRequired (..)
  , CloneTimeoutError (..)
  ) where

import Restyler.Prelude

import Restyler.App.Class
import Restyler.Git
import Restyler.GitHub.Api
import Restyler.GitHub.Repository
import Restyler.JobEnv
import Restyler.Options.PullRequest
import Restyler.Statsd (HasStatsClient)
import Restyler.Statsd qualified as Statsd
import Restyler.Wiki qualified as Wiki

data PlanUpgradeRequired = PlanUpgradeRequired Text (Maybe URL)
  deriving stock (Eq, Show)

instance Exception PlanUpgradeRequired where
  displayException (PlanUpgradeRequired message mUpgradeUrl) =
    unpack
      $ message
      <> "\nFor additional help, please see: "
      <> Wiki.commonError "Plan Upgrade Required"
      <> maybe
        ""
        (("\nYou can upgrade your plan at " <>) . getUrl)
        mUpgradeUrl

restylerSetup
  :: ( HasCallStack
     , MonadUnliftIO m
     , MonadLogger m
     , MonadSystem m
     , MonadExit m
     , MonadProcess m
     , MonadReader env m
     , HasStatsClient env
     , HasGitHubToken env
     , HasWorkingDirectory env
     )
  => JobEnv
  -> PullRequestOption
  -> m ()
restylerSetup env pr = do
  when env.repoDisabled
    $ exitWithInfo
    $ fromString
    $ "This repository has been disabled for possible abuse."
    <> " If you believe this is an error, please reach out to"
    <> " support@restyled.io"

  for_ env.planRestriction $ \planRestriction -> do
    throwIO $ PlanUpgradeRequired planRestriction env.planUpgradeUrl

  logInfo "Cloning repository"
  token <- view githubTokenL
  dir <- view workingDirectoryL
  wrapClone
    $ gitCloneBranchByRef
      ("pull/" <> show pr.number <> "/head")
      ("pull-" <> show pr.number)
      ( unpack
          $ "https://x-access-token:"
          <> unGitHubToken token
          <> "@github.com/"
          <> pr.repo.owner
          <> "/"
          <> pr.repo.repo
          <> ".git"
      )
      dir

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
