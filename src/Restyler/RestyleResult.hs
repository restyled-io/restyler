module Restyler.RestyleResult
  ( RestyleResult (..)
  , RestyleSkipped (..)
  , runRestyle
  , setRestylerResultOutputs
  , renderSkipped
  ) where

import Restyler.Prelude

import Data.Text qualified as T
import Restyler.Config
import Restyler.GHA.Output
import Restyler.GitHub.PullRequest
import Restyler.Ignore
import Restyler.RestyledPullRequest
import Restyler.RestylerResult

data RestyleResult pr
  = RestyleSkipped Config pr RestyleSkipped
  | RestyleSuccessNoDifference Config pr [RestylerResult]
  | RestyleSuccessDifference Config pr [RestylerResult]

runRestyle
  :: (Monad m, HasCallStack)
  => Config
  -> pr
  -> (HasCallStack => m [RestylerResult])
  -> m (RestyleResult pr)
runRestyle config pr run = do
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

setRestylerResultOutputs
  :: (MonadIO m, MonadReader env m, HasGitHubOutput env)
  => RestyleResult PullRequest
  -> m ()
setRestylerResultOutputs = \case
  RestyleSuccessDifference config pr results -> do
    let details = restyledPullRequestDetails config pr results
    appendGitHubOutput
      $ unlines
        [ "differences=true"
        , "restyled-base=" <> details.base
        , "restyled-head=" <> details.head
        , "restyled-title=" <> details.title
        , "restyled-body<<EOM"
        , details.body
        , "EOM"
        , "restyled-labels=" <> mcsv details.labels
        , "restyled-reviewers=" <> mcsv details.reviewers
        , "restyled-team-reviewers=" <> mcsv details.teamReviewers
        ]
  _ -> appendGitHubOutput "differences=false"
 where
  mcsv :: Maybe (NonEmpty Text) -> Text
  mcsv = maybe "" (T.intercalate "," . toList)

renderSkipped :: RestyleSkipped -> Text
renderSkipped = \case
  RestyleNotEnabled -> "not enabled"
  RestylePullRequestClosed -> "PR closed"
  RestyleIgnored reason -> case reason of
    IgnoredByAuthor {} -> "ignore author"
    IgnoredByBranch {} -> "ignore branch"
    IgnoredByLabels {} -> "ignore labels"
