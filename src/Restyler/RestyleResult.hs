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
  | RestyleSkippedIgnored IgnoredReason
  | Restyled [RestylerResult]

logRestyleResult :: MonadLogger m => RestyleResult -> m ()
logRestyleResult = \case
  RestyleSkippedClosed -> logInfo $ "Restyle skipped" :# ["reason" .= t "closed"]
  RestyleSkippedIgnored reason -> logInfo $ "Restyle skipped" :# ["reason" .= show @Text reason]
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
  :: (MonadIO m, MonadReader env m, HasGitHubOutput env)
  => PullRequest
  -> RestyleResult
  -> m Bool
setRestylerResultOutputs pr result = do
  output <- view githubOutputL

  let write = liftIO . appendFileText output.unwrap

  case result of
    Restyled results
      | any restylerCommittedChanges results -> do
          write
            $ unlines
              [ "differences=true"
              , "restyle-branch-name=restyled/" <> pr.head.ref
              , "restyle-pr-title=Restyle " <> pr.title
              , "restyle-pr-body<<EOM"
              , Content.pullRequestDescription Nothing pr.number results
              , "EOM"
              ]
          pure True
    _ -> False <$ write "differences=false"

t :: Text -> Text
t = id
