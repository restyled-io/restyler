-- |
--
-- <https://developer.github.com/v3/pulls/review_requests>
--
module GitHub.Endpoints.PullRequests.ReviewRequests
    ( RequestReview(..)
    , requestOneReviewer
    , ReviewRequest(..)
    , createReviewRequest
    , createReviewRequestR
    )
where

import Prelude

import Data.Aeson
import GitHub.Data
import GitHub.Request

data RequestReview = RequestReview
    { requestReviewReviewers :: [Name User]
    , requestReviewTeamReviewers :: [Name Team]
    }

requestOneReviewer :: Name User -> RequestReview
requestOneReviewer reviewer = RequestReview
    { requestReviewReviewers = [reviewer]
    , requestReviewTeamReviewers = []
    }

instance ToJSON RequestReview where
    toJSON rr = object
        [ "reviewers" .= requestReviewReviewers rr
        , "team_reviewers" .= requestReviewTeamReviewers rr
        ]

newtype ReviewRequest = ReviewRequest
    { reviewRequestUrl :: URL
    }

instance FromJSON ReviewRequest where
    parseJSON = withObject "ReviewRequest" $ \o -> ReviewRequest <$> o .: "url"

createReviewRequest
    :: Auth
    -> Name Owner
    -> Name Repo
    -> IssueNumber
    -> RequestReview
    -> IO (Either Error ReviewRequest)
createReviewRequest auth user repo pull =
    executeRequest auth . createReviewRequestR user repo pull

createReviewRequestR
    :: Name Owner
    -> Name Repo
    -> IssueNumber
    -> RequestReview
    -> Request 'RW ReviewRequest
createReviewRequestR user repo pull = command Post paths . encode
  where
    paths =
        [ "repos"
        , toPathPart user
        , toPathPart repo
        , "pulls"
        , toPathPart pull
        , "requested_reviewers"
        ]
