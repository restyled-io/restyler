module Restyler.RestyleResult
  ( RestyleResult (..)
  , RestyleSkipped (..)
  , runRestyle
  , logRestyleResult
  , setRestylerResultOutputs
  ) where

import Restyler.Prelude

import Data.Aeson (ToJSON)
import Restyler.Config
import Restyler.Content qualified as Content
import Restyler.GHA.Output
import Restyler.GitHub.PullRequest
import Restyler.Ignore
import Restyler.RestylerResult
import UnliftIO.Exception (handleAny)

data RestyleResult pr
  = RestyleFailedEarly SomeException
  | RestyleFailed Config pr SomeException
  | RestyleSkipped Config pr RestyleSkipped
  | RestyleSuccessNoDifference Config pr [RestylerResult]
  | RestyleSuccessDifference Config pr [RestylerResult]

runRestyle
  :: MonadUnliftIO m => Config -> pr -> m [RestylerResult] -> m (RestyleResult pr)
runRestyle config pr run = do
  handleAny (pure . RestyleFailed config pr) $ do
    results <- run
    pure
      $ if any restylerCommittedChanges results
        then RestyleSuccessDifference config pr results
        else RestyleSuccessNoDifference config pr results

data RestyleSkipped
  = RestyleNotEnabled
  | RestylePullRequestClosed
  | RestyleIgnored IgnoredReason
  deriving stock (Generic)
  deriving anyclass (ToJSON)

logRestyleResult :: RestyleResult pr -> m ()
logRestyleResult = error "TODO" -- \case
-- RestyleSkippedDisabled -> logInfo $ "Restyle skipped" :# ["reason" .= t "disabled"]
-- RestyleSkippedClosed -> logInfo $ "Restyle skipped" :# ["reason" .= t "closed"]
-- RestyleSkippedIgnored reason -> logInfo $ "Restyle skipped" :# ["reason" .= show @Text reason]
-- Restyled _ results -> traverse_ logRestylerResult results

-- logRestylerResult :: MonadLogger m => RestylerResult -> m ()
-- logRestylerResult RestylerResult {..} =
--   case rrOutcome of
--     ChangesCommitted changed sha -> do
--       logInfo
--         $ "Changes committed"
--         :# [ "restyler" .= rName rrRestyler
--            , "paths" .= length changed
--            , "sha" .= sha
--            ]
--     x -> logDebug $ "" :# ["result" .= x]

setRestylerResultOutputs
  :: (MonadIO m, MonadReader env m, HasGitHubOutput env)
  => PullRequest
  -> RestyleResult pr
  -> m ()
setRestylerResultOutputs pr = \case
  RestyleSuccessDifference _config _pr results ->
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

-- t :: Text -> Text
-- t = id
