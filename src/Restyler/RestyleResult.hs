module Restyler.RestyleResult
  ( RestyleResult (..)
  , RestyleSkipped (..)
  , runRestyle
  , setRestylerResultOutputs
  ) where

import Restyler.Prelude

import Data.Text qualified as T
import Restyler.Config
import Restyler.GHA.Output
import Restyler.GHA.Outputs
import Restyler.GitHub.PullRequest
import Restyler.Ignore
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
setRestylerResultOutputs =
  appendGitHubOutputs . \case
    RestyleSuccessDifference config pr results ->
      let outputs = restylerOutputs config pr results
      in  [ "differences=true"
          , "restyled-base=" <> outputs.base
          , "restyled-head=" <> outputs.head
          , "restyled-title=" <> outputs.title
          , "restyled-body<<EOM\n" <> outputs.body <> "\nEOM"
          , "restyled-labels=" <> mcsv outputs.labels
          , "restyled-reviewers=" <> mcsv outputs.reviewers
          , "restyled-team-reviewers=" <> mcsv outputs.teamReviewers
          ]
    _ -> ["differences=false"]
 where
  mcsv :: Maybe (NonEmpty Text) -> Text
  mcsv = maybe "" (T.intercalate "," . toList)
