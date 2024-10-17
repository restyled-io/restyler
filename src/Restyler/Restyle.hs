-- | @restyle PATH [PATH...]@
--
-- Module      : Restyler.Restyle
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Restyle
  ( NullPullRequest (..)
  , run
  ) where

import Restyler.Prelude

import Restyler.Config
import Restyler.GitHub.PullRequest
import Restyler.Ignore
import Restyler.Monad.Directory
import Restyler.Monad.Docker (MonadDocker)
import Restyler.Monad.DownloadFile
import Restyler.Monad.Git (MonadGit (..))
import Restyler.Monad.ReadFile
import Restyler.Monad.WriteFile
import Restyler.Options
import Restyler.RestyleResult
import Restyler.Restyler
import Restyler.Restyler.Run
import Restyler.RestylerResult

run
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadDownloadFile m
     , MonadDirectory m
     , MonadReadFile m
     , MonadWriteFile m
     , MonadGit m
     , MonadDocker m
     , MonadReader env m
     , HasDryRun env
     , HasHostDirectory env
     , HasImageCleanup env
     , HasManifest env
     , HasNoCommit env
     , HasNoClean env
     , HasNoPull env
     , HasRestrictions env
     , HasPullRequestState pr
     , HasAuthor pr
     , HasBaseRef pr
     , HasLabelNames pr
     , HasCallStack
     )
  => pr
  -> [FilePath]
  -> m RestyleResult
run pr paths = do
  config <- loadConfig

  let mIgnoredReason =
        getIgnoredReason
          config
          (getAuthor pr)
          (getBaseRef pr)
          (getLabelNames pr)

  case (cEnabled config, getPullRequestState pr) of
    (False, _) -> do
      pure $ RestyleSkipped RestyleNotEnabled
    (True, PullRequestClosed) ->
      pure $ RestyleSkipped RestylePullRequestClosed
    (True, PullRequestOpen)
      | Just reason <- mIgnoredReason ->
          pure $ RestyleSkipped $ RestyleIgnored reason
    (True, PullRequestOpen) -> do
      mresults <- runRestylers config paths

      -- Overall result is logged in CLI, log individual results here
      for_ mresults $ traverse_ $ \result -> do
        logDebug
          $ "RestylerResult"
          :# [ "restyler" .= rName (result.restyler)
             , "restyled" .= result.restyled
             , "sha" .= result.sha
             ]

      noClean <- asks getNoClean
      unless noClean gitClean

      pure $ maybe RestyleNoDifference (const RestyleDifference) mresults