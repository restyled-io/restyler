module Restyler.Config.RequestReview
  ( RequestReviewConfig
  , determineReviewer
  ) where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Types (typeMismatch)
import GitHub.Data (User, toPathPart)
import Restyler.Config.ExpectedKeys
import Restyler.PullRequest

data RequestReviewFrom
  = RequestReviewFromNone
  | RequestReviewFromAuthor
  | RequestReviewFromOwner
  | RequestReviewFrom (Name User)
  deriving stock (Eq, Show, Generic)

instance FromJSON RequestReviewFrom where
  parseJSON = withText "RequestReviewFrom" $ pure . readRequestReviewFrom

instance ToJSON RequestReviewFrom where
  toJSON RequestReviewFromNone = String "none"
  toJSON RequestReviewFromAuthor = String "author"
  toJSON RequestReviewFromOwner = String "owner"
  toJSON (RequestReviewFrom name) = String $ toPathPart name

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

-- brittany-disable-next-binding

instance FromJSON RequestReviewConfig where
  parseJSON (String t) =
    pure $ bothFrom $ readRequestReviewFrom t
  parseJSON (Object o) = do
    validateObjectKeys ["origin", "forked"] o
    RequestReviewConfig
      <$> o
      .:? "origin"
      .!= RequestReviewFromAuthor
      <*> o
      .:? "forked"
      .!= RequestReviewFromNone
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
  -> Maybe (Name User)
determineReviewer pr RequestReviewConfig {..} =
  (`reviewerFor` pr) $ bool rrcOrigin rrcForked $ pullRequestIsFork pr

reviewerFor :: RequestReviewFrom -> PullRequest -> Maybe (Name User)
reviewerFor RequestReviewFromNone = const Nothing
reviewerFor RequestReviewFromAuthor = Just . pullRequestUserLogin
reviewerFor RequestReviewFromOwner = Just . coerceName . pullRequestOwnerName
reviewerFor (RequestReviewFrom name) = const $ Just name

-- TODO: centralize this?
coerceName :: Name a -> Name b
coerceName = mkName Proxy . untagName
