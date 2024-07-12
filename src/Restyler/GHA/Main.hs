{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Restyler.GHA.Main
  ( main
  ) where

import Restyler.Prelude

import Data.Text qualified as T
import Restyler.App (AppT, runAppT)
import Restyler.App.Class (MonadProcess (..))
import Restyler.Config
import Restyler.GHA.Options
import Restyler.Git (MonadGit (..))
import Restyler.GitHub qualified as GitHub
import Restyler.GitHub.Api
import Restyler.GitHub.PullRequest.File
import Restyler.GitHubTokenOption
import Restyler.HostDirectoryOption
import Restyler.ImageCleanupOption
import Restyler.LogSettingsOption
import Restyler.ManifestOption
import Restyler.PullRequestNumberOption
import Restyler.RepositoryOption
import Restyler.Restrictions
import Restyler.Restyler
import Restyler.Restyler.Run (runRestylers)
import Restyler.RestylerResult
import UnliftIO.Exception (handleAny)

data App = App
  { logger :: Logger
  , githubToken :: GitHubToken
  , restrictions :: Restrictions
  , manifest :: ManifestOption
  , hostDirectory :: HostDirectoryOption
  , imageCleanup :: ImageCleanupOption
  }

instance HasLogger App where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

instance HasGitHubToken App where
  githubTokenL = lens (.githubToken) $ \x y -> x {githubToken = y}

instance HasRestrictions App where
  restrictionsL = lens (.restrictions) $ \x y -> x {restrictions = y}

instance HasManifestOption App where
  manifestOptionL = lens (.manifest) $ \x y -> x {manifest = y}

instance HasHostDirectoryOption App where
  hostDirectoryOptionL = lens (.hostDirectory) $ \x y -> x {hostDirectory = y}

instance HasImageCleanupOption App where
  imageCleanupOptionL = lens (.imageCleanup) $ \x y -> x {imageCleanup = y}

instance MonadUnliftIO m => MonadGit (AppT App m) where
  gitPush branch = callProcess "git" ["push", "origin", branch]
  gitPushForce branch =
    callProcess "git" ["push", "--force", "origin", branch]
  gitDiffNameOnly mRef = do
    let args = ["diff", "--name-only"] <> maybeToList mRef
    map unpack . lines . pack <$> readProcess "git" args
  gitFormatPatch mRef = do
    let args = ["format-patch", "--stdout"] <> maybeToList mRef
    pack <$> readProcess "git" args
  gitCommitAll msg = do
    callProcess "git" ["commit", "-a", "--message", msg]
    unpack
      . T.dropWhileEnd isSpace
      . pack
      <$> readProcess
        "git"
        ["rev-parse", "HEAD"]
  gitCheckout branch = do
    callProcess "git" ["checkout", "--no-progress", "-b", branch]

main :: IO ()
main = do
  options <- getOptions

  withLogger (unLogSettingsOption options.logSettings) $ \logger -> do
    let app =
          App
            { logger = logger
            , githubToken = getGitHubTokenOption options.githubToken
            , restrictions = options.restrictions
            , manifest = toManifestOption Nothing
            , hostDirectory = toHostDirectoryOption Nothing
            , imageCleanup = toImageCleanupOption False
            }

    runAppT app $ handleAny logExit $ do
      config <- loadConfig
      logDebug $ "Config" :# objectToPairs config

      let repoPR =
            (,)
              <$> unRepositoryOption options.repository
              <*> unPullRequestNumberOption options.pullRequest

      paths <- case repoPR of
        Nothing -> error "CLI paths"
        Just (repo, pr) -> do
          pullRequest <- GitHub.getPullRequest repo pr
          logInfo $ "Handling PR" :# objectToPairs pullRequest

          -- TODO
          -- check draft
          -- check closed
          -- check ignores

          mapMaybe pullRequestFileToChangedPath
            <$> GitHub.getPullRequestFiles repo pr

      results <- runRestylers config paths

      for_ results $ \RestylerResult {..} ->
        case rrOutcome of
          ChangesCommitted changed sha -> do
            logInfo
              $ "Changes committed"
              :# [ "restyler" .= rName rrRestyler
                 , "paths" .= length changed
                 , "sha" .= sha
                 ]
          x -> logDebug $ "Outcome" :# ["restyler" .= rName rrRestyler] <> objectToPairs x

logExit :: (MonadIO m, MonadLogger m) => SomeException -> m a
logExit ex = do
  logError $ pack (displayException ex) :# []
  exitFailure
