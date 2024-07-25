module Restyler.RestyleResult
  ( RestyleResult (..)
  , RestyleSkipped (..)
  , runRestyle
  , setRestylerResultOutputs
  ) where

import Restyler.Prelude

import Data.Text qualified as T
import GitHub qualified
import Restyler.Config
import Restyler.Config.RequestReview
import Restyler.Content (pullRequestDescription)
import Restyler.GHA.Output
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
    RestyleSkipped _ pr _ ->
      [ "differences=false"
      , "restyled-base=" <> pr.head.ref
      , "restyled-head=restyled/" <> pr.head.ref
      , "skipped=true"
      ]
    RestyleSuccessNoDifference _ pr _ ->
      [ "differences=false"
      , "restyled-base=" <> pr.head.ref
      , "restyled-head=restyled/" <> pr.head.ref
      ]
    RestyleSuccessDifference config pr results patch ->
      -- NB. The EOMs are safe because: (1) git-patch is always guaranteed a
      -- column of whitespace, so if it itself contains EOM, that'd be " EOM"
      -- and (2) we control the body content and wouldn't put an "EOM" there.
      let
        body =
          maybe
            ""
            (pullRequestDescription pr.number)
            $ nonEmpty
            $ filter restylerCommittedChanges results

        labels =
          nonEmpty
            $ map GitHub.untagName
            $ toList
            $ cLabels config

        reviewers =
          fmap (pure . GitHub.untagName)
            $ determineReviewer pr
            $ cRequestReview config
      in
        [ "differences=true"
        , "git-patch<<EOM\n" <> patch <> "\nEOM"
        , "restyled-base=" <> pr.head.ref
        , "restyled-head=restyled/" <> pr.head.ref
        , "restyled-title=Restyle " <> pr.title
        , "restyled-body<<EOM\n" <> body <> "\nEOM"
        , "restyled-labels=" <> mcsv labels
        , "restyled-reviewers=" <> mcsv reviewers
        , "restyled-team-reviewers=" -- TODO
        ]
 where
  mcsv :: Maybe (NonEmpty Text) -> Text
  mcsv = maybe "" (T.intercalate "," . toList)
