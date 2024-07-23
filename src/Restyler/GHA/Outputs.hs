module Restyler.GHA.Outputs
  ( RestylerOutputs (..)
  , restylerOutputs
  ) where

import Restyler.Prelude

import GitHub qualified
import Restyler.Config
import Restyler.Config.RequestReview
import Restyler.Content qualified as Content
import Restyler.GitHub.PullRequest
import Restyler.Options.Repository
import Restyler.RestylerResult

data RestylerOutputs = RestylerOutputs
  { repo :: RepositoryOption
  , title :: Text
  , body :: Text
  , base :: Text
  , head :: Text
  , labels :: Maybe (NonEmpty Text)
  , reviewers :: Maybe (NonEmpty Text)
  , teamReviewers :: Maybe (NonEmpty Text)
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

restylerOutputs
  :: Config
  -> PullRequest
  -> [RestylerResult]
  -> RestylerOutputs
restylerOutputs config pr results =
  RestylerOutputs
    { repo =
        RepositoryOption
          { owner = pr.base.repo.owner.login
          , repo = pr.base.repo.name
          }
    , title = "Restyle " <> pr.title
    , body = maybe "" toBody $ nonEmpty results
    , base = pr.head.ref
    , head = "restyled/" <> pr.head.ref
    , labels = nonEmpty $ map GitHub.untagName $ toList $ cLabels config
    , reviewers =
        pure . GitHub.untagName <$> determineReviewer pr (cRequestReview config)
    , teamReviewers = Nothing
    }
 where
  toBody = Content.pullRequestDescription Nothing pr.number
