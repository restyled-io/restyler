-- |
--
-- - Fetch PR details
-- - Check closed, ignore, etc
-- - Run @RestyleLocal@ with commits
module Restyler.Commands.RestyleGHA
  ( run
  ) where

import Restyler.Prelude

import Data.List.NonEmpty qualified as NE
import Restyler.App (runAppT)
import Restyler.App.Class (MonadProcess (..))
import Restyler.Commands.RestyleLocal qualified as RestyleLocal
import Restyler.Config (loadConfig)
import Restyler.GitHub.Api
import Restyler.GitHub.PullRequest.File
import Restyler.GitHub.Repository
import Restyler.LogSettingsOption
import Restyler.RestylerResult

data Options = Options
  { logSettings :: LogSettingsOption
  , githubToken :: GitHubToken
  }

instance HasLogSettingsOption Options where
  logSettingsOptionL = lens (.logSettings) $ \x y -> x {logSettings = y}

instance HasGitHubToken Options where
  githubTokenL = lens (.githubToken) $ \x y -> x {githubToken = y}

data App = App
  { logger :: Logger
  , options :: Options
  }

optionsL :: Lens' App Options
optionsL = lens (.options) $ \x y -> x {options = y}

instance HasLogger App where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

instance HasGitHubToken App where
  githubTokenL = optionsL . githubTokenL

run :: Repository -> Int -> m (Maybe [RestylerResult])
run repo pr = do
  options <- getOptions

  let logSettings = options ^. logSettingsOptionL . to resolveLogSettings

  withLogger logSettings $ \logger -> do
    let app =
          App
            { logger = logger
            , options = options
            }

    runAppT app $ do
      config <- loadConfig
      logDebug $ "Config" :# objectToPairs config

      pullRequest <- getPullRequest repo pr
      logInfo $ "Handling PR" :# objectToPairs pullRequest

      -- TODO
      -- check draft
      -- check closed
      -- check ignores

      mPaths <-
        NE.nonEmpty
          . mapMaybe pullRequestFileToChangedPath
          <$> getPullRequestFiles repo pr

      traverse RestyleLocal.run mPaths

--         traverse_ (runRestylers config . toList) mPaths
--       RestylePaths paths -> runRestylers config $ toList paths

--     for_ results $ \RestylerResult {..} ->
--       case rrOutcome of
--         ChangesCommitted changed sha -> do
--           logInfo
--             $ "Changes committed"
--             :# [ "restyler" .= rName rrRestyler
--                , "paths" .= length changed
--                , "sha" .= sha
--                ]
--         x -> logDebug $ "Outcome" :# ["restyler" .= rName rrRestyler] <> objectToPairs x

-- data App = App
--   { logger :: Logger
--   , restrictions :: Restrictions
--   , manifest :: ManifestOption
--   , hostDirectory :: HostDirectoryOption
--   , imageCleanup :: ImageCleanupOption
--   }

-- instance HasLogger App where
--   loggerL = lens (.logger) $ \x y -> x {logger = y}

-- instance HasRestrictions App where
--   restrictionsL = lens (.restrictions) $ \x y -> x {restrictions = y}

-- instance HasManifestOption App where
--   manifestOptionL = lens (.manifest) $ \x y -> x {manifest = y}

-- instance HasHostDirectoryOption App where
--   hostDirectoryOptionL = lens (.hostDirectory) $ \x y -> x {hostDirectory = y}

-- instance HasImageCleanupOption App where
--   imageCleanupOptionL = lens (.imageCleanup) $ \x y -> x {imageCleanup = y}

-- instance MonadUnliftIO m => MonadGit (AppT App m) where
--   gitPush branch = callProcess "git" ["push", "origin", branch]
--   gitPushForce branch =
--     callProcess "git" ["push", "--force", "origin", branch]
--   gitDiffNameOnly mRef = do
--     let args = ["diff", "--name-only"] <> maybeToList mRef
--     map unpack . lines . pack <$> readProcess "git" args
--   gitFormatPatch mRef = do
--     let args = ["format-patch", "--stdout"] <> maybeToList mRef
--     pack <$> readProcess "git" args
--   gitCommitAll msg = do
--     callProcess "git" ["commit", "-a", "--message", msg]
--     unpack
--       . T.dropWhileEnd isSpace
--       . pack
--       <$> readProcess
--         "git"
--         ["rev-parse", "HEAD"]
--   gitCheckout branch = do
--     callProcess "git" ["checkout", "--no-progress", "-b", branch]
