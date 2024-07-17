module Restyler.RestyleResult
  ( RestyleResult (..)
  , logRestyleResult
  , logRestylerResult
  , setRestylerResultOutputs
  ) where

import Restyler.Prelude

import Restyler.Content qualified as Content
import Restyler.GHA.Output
import Restyler.GitHub.PullRequest
import Restyler.Ignore
import Restyler.Restyler
import Restyler.RestylerResult

data RestyleResult pr
  = RestyleSkippedDisabled
  | RestyleSkippedClosed
  | RestyleSkippedIgnored IgnoredReason
  | Restyled pr [RestylerResult]

logRestyleResult :: MonadLogger m => RestyleResult pr -> m ()
logRestyleResult = \case
  RestyleSkippedDisabled -> logInfo $ "Restyle skipped" :# ["reason" .= t "disabled"]
  RestyleSkippedClosed -> logInfo $ "Restyle skipped" :# ["reason" .= t "closed"]
  RestyleSkippedIgnored reason -> logInfo $ "Restyle skipped" :# ["reason" .= show @Text reason]
  Restyled _ results -> traverse_ logRestylerResult results

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
  :: (MonadIO m, MonadReader env m, HasGitHubOutput env)
  => PullRequest
  -> RestyleResult pr
  -> m ()
setRestylerResultOutputs pr = \case
  Restyled _ results
    | any restylerCommittedChanges results ->
        appendGitHubOutput
          $ unlines
            [ "differences=true"
            , "restyled-branch-name=restyled/" <> pr.head.ref
            , "restyled-pr-title=Restyle " <> pr.title
            , "restyled-pr-body<<EOM"
            , Content.pullRequestDescription Nothing pr.number results
            , "EOM"
            ]
  _ -> appendGitHubOutput "differences=false"

t :: Text -> Text
t = id
