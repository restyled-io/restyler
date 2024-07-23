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
import Restyler.Git
import Restyler.GitHub.PullRequest
import Restyler.Ignore
import Restyler.RestylerResult

data RestyleResult pr
  = RestyleSkipped Config pr RestyleSkipped
  | RestyleSuccessNoDifference Config pr [RestylerResult]
  | RestyleSuccessDifference Config pr [RestylerResult] Text

runRestyle
  :: (MonadGit m, HasHeadSha pr, HasCallStack)
  => Config
  -> pr
  -> (HasCallStack => m [RestylerResult])
  -> m (RestyleResult pr)
runRestyle config pr run = do
  results <- run
  if any restylerCommittedChanges results
    then do
      patch <- gitFormatPatch $ Just $ unpack $ getHeadSha pr
      pure $ RestyleSuccessDifference config pr results patch
    else pure $ RestyleSuccessNoDifference config pr results

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
    RestyleSkipped config pr _ ->
      let outputs = restylerOutputs config pr []
      in  [ "differences=false"
          , "restyled-base=" <> outputs.base
          , "restyled-head=" <> outputs.head
          , "skipped=true"
          ]
    RestyleSuccessNoDifference config pr _ ->
      let outputs = restylerOutputs config pr []
      in  [ "differences=false"
          , "restyled-base=" <> outputs.base
          , "restyled-head=" <> outputs.head
          ]
    RestyleSuccessDifference config pr results patch ->
      let outputs = restylerOutputs config pr results
      in  [ "differences=true"
          , "git-patch<<EOM\n" <> patch <> "\nEOM"
          , "restyled-base=" <> outputs.base
          , "restyled-head=" <> outputs.head
          , "restyled-title=" <> outputs.title
          , "restyled-body<<EOM\n" <> outputs.body <> "\nEOM"
          , "restyled-labels=" <> mcsv outputs.labels
          , "restyled-reviewers=" <> mcsv outputs.reviewers
          , "restyled-team-reviewers=" <> mcsv outputs.teamReviewers
          ]
 where
  mcsv :: Maybe (NonEmpty Text) -> Text
  mcsv = maybe "" (T.intercalate "," . toList)
