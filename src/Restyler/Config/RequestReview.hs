-- TODO
{-# LANGUAGE RecordWildCards #-}

module Restyler.Config.RequestReview
  ( RequestReviewConfig
  , determineReviewer
  ) where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Types (typeMismatch)
import GitHub qualified
import Restyler.Config.ExpectedKeys
import Restyler.GitHub.PullRequest

data RequestReviewFrom
  = RequestReviewFromNone
  | RequestReviewFromAuthor
  | RequestReviewFromOwner
  | RequestReviewFrom (GitHub.Name GitHub.User)
  deriving stock (Eq, Show, Generic)

instance FromJSON RequestReviewFrom where
  parseJSON = withText "RequestReviewFrom" $ pure . readRequestReviewFrom

instance ToJSON RequestReviewFrom where
  toJSON =
    String . \case
      RequestReviewFromNone -> "none"
      RequestReviewFromAuthor -> "author"
      RequestReviewFromOwner -> "owner"
      RequestReviewFrom name -> GitHub.toPathPart name

readRequestReviewFrom :: Text -> RequestReviewFrom
readRequestReviewFrom = \case
  "none" -> RequestReviewFromNone
  "author" -> RequestReviewFromAuthor
  "owner" -> RequestReviewFromOwner
  x -> RequestReviewFrom $ mkName Proxy x

data RequestReviewConfig = RequestReviewConfig
  { rrcOrigin :: RequestReviewFrom
  , rrcForked :: RequestReviewFrom
  }
  deriving stock (Eq, Show, Generic)

bothFrom :: RequestReviewFrom -> RequestReviewConfig
bothFrom x = RequestReviewConfig {rrcOrigin = x, rrcForked = x}

instance FromJSON RequestReviewConfig where
  parseJSON (String t) =
    pure $ bothFrom $ readRequestReviewFrom t
  parseJSON (Object o) = do
    validateObjectKeys ["origin", "forked"] o
    RequestReviewConfig
      <$> (o .:? "origin" .!= RequestReviewFromAuthor)
      <*> (o .:? "forked" .!= RequestReviewFromNone)
  parseJSON x =
    typeMismatch
      "Invalid type for RequestReview. Expected String or Object."
      x

instance ToJSON RequestReviewConfig where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

determineReviewer
  :: PullRequest
  -- ^ The Original PR
  -> RequestReviewConfig
  -> Maybe (GitHub.Name GitHub.User)
determineReviewer pr RequestReviewConfig {..} =
  (`reviewerFor` pr) $ bool rrcOrigin rrcForked pullRequestIsFork
 where
  pullRequestIsFork = pr.head.repo.owner.login /= pr.base.repo.owner.login

reviewerFor
  :: RequestReviewFrom -> PullRequest -> Maybe (GitHub.Name GitHub.User)
reviewerFor RequestReviewFromNone _ = Nothing
reviewerFor RequestReviewFromAuthor pr = Just $ GitHub.mkName Proxy pr.user.login
reviewerFor RequestReviewFromOwner pr = Just $ GitHub.mkName Proxy pr.base.repo.owner.login
reviewerFor (RequestReviewFrom name) _ = Just name
