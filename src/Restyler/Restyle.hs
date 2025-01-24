-- | @restyle PATH [PATH...]@
--
-- Module      : Restyler.Restyle
-- Copyright   : (c) 2025 Patrick Brisbin
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
import Restyler.RestyleResult
import Restyler.Restyler
import Restyler.Restyler.Run
import Restyler.RestylerResult

run
  :: ( HasAuthor pr
     , HasBaseRef pr
     , HasCallStack
     , HasCommitTemplate env
     , HasDryRun env
     , HasEnabled env
     , HasExclude env
     , HasHostDirectory env
     , HasIgnores env
     , HasImageCleanup env
     , HasLabelNames pr
     , HasManifest env
     , HasNoClean env
     , HasNoCommit env
     , HasNoPull env
     , HasPullRequestState pr
     , HasRemoteFiles env
     , HasRestrictions env
     , HasRestylerOverrides env
     , HasRestylersVersion env
     , MonadDirectory m
     , MonadDocker m
     , MonadDownloadFile m
     , MonadGit m
     , MonadLogger m
     , MonadReadFile m
     , MonadReader env m
     , MonadUnliftIO m
     , MonadWriteFile m
     )
  => pr
  -> [FilePath]
  -> m RestyleResult
run pr paths = do
  enabled <- asks getEnabled
  ignores <- asks getIgnores

  let mIgnoredReason =
        getIgnoredReason
          ignores
          (getAuthor pr)
          (getBaseRef pr)
          (getLabelNames pr)

  case (enabled, getPullRequestState pr) of
    (False, _) -> do
      pure $ RestyleSkipped RestyleNotEnabled
    (True, PullRequestClosed) ->
      pure $ RestyleSkipped RestylePullRequestClosed
    (True, PullRequestOpen)
      | Just reason <- mIgnoredReason ->
          pure $ RestyleSkipped $ RestyleIgnored reason
    (True, PullRequestOpen) -> do
      mresults <- runRestylers paths

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
