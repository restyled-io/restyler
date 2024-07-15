module Restyler.RestyleResult
  ( RestyleResult (..)
  , logRestyleResult
  , logRestylerResult
  , setRestylerResultOutputs
  ) where

import Restyler.Prelude

import Restyler.Content qualified as Content
import Restyler.GHA
import Restyler.GitHub.PullRequest
import Restyler.Ignore
import Restyler.Restyler
import Restyler.RestylerResult

data RestyleResult
  = RestyleSkippedClosed
  | RestyleSkippedDraft
  | RestyleSkippedIgnored IgnoredReason
  | RestyleSkippedNoRestylers
  | RestyleSkippedNoPaths
  | Restyled [RestylerResult]

logRestyleResult :: MonadLogger m => RestyleResult -> m ()
logRestyleResult = \case
  RestyleSkippedClosed -> logInfo $ "Restyle skipped" :# ["reason" .= t "closed"]
  RestyleSkippedDraft -> logInfo $ "Restyle skipped" :# ["reason" .= t "closed"]
  RestyleSkippedIgnored reason -> logInfo $ "Restyle skipped" :# ["reason" .= show @Text reason]
  RestyleSkippedNoRestylers -> logInfo $ "Restyle skipped" :# ["reason" .= t "No Restylers"]
  RestyleSkippedNoPaths -> logInfo $ "Restyle skipped" :# ["reason" .= t "No paths"]
  Restyled results -> traverse_ logRestylerResult results

logRestylerResult :: MonadLogger m => RestylerResult -> m ()
logRestylerResult RestylerResult {..} =
  case rrOutcome of
    ChangesCommitted changed sha -> do
      logInfo
        $ "Changes committed"
        :# [ "restyler" .= rName rrRestyler
           , "paths" .= length changed
           , "sha" .= sha
           ]
    x -> logDebug $ "" :# ["result" .= x]

setRestylerResultOutputs
  :: (MonadIO m, MonadLogger m, MonadReader env m, HasGitHubOutput env)
  => PullRequest
  -> RestyleResult
  -> m Bool
setRestylerResultOutputs pr = \case
  Restyled results | any restylerCommittedChanges results -> setDifferences pr results
  _ -> setNoDifferences

setNoDifferences
  :: (MonadIO m, MonadLogger m, MonadReader env m, HasGitHubOutput env) => m Bool
setNoDifferences = False <$ setGitHubOutput "differences" "false"

setDifferences
  :: (MonadIO m, MonadLogger m, MonadReader env m, HasGitHubOutput env)
  => PullRequest
  -> [RestylerResult]
  -> m Bool
setDifferences pr results =
  True <$ do
    setGitHubOutput "differences" "true"
    setGitHubOutput "restyle-branch-name" $ "restyled/" <> pr.head.ref
    setGitHubOutput "restyle-pr-title" $ "Restyle " <> pr.title
    setGitHubOutputLn "restyle-pr-body"
      $ Content.pullRequestDescription Nothing pr.number results

t :: Text -> Text
t = id
