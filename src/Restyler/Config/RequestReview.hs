-- |
--
-- Module      : Restyler.Config.RequestReview
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.RequestReview
  ( RequestReviewConfig
  ) where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Types (typeMismatch)
import Restyler.Config.ExpectedKeys

data RequestReviewFrom
  = RequestReviewFromNone
  | RequestReviewFromAuthor
  | RequestReviewFromOwner
  | RequestReviewFrom Text
  deriving stock (Eq, Show, Generic)

instance FromJSON RequestReviewFrom where
  parseJSON = withText "RequestReviewFrom" $ pure . readRequestReviewFrom

instance ToJSON RequestReviewFrom where
  toJSON =
    String . \case
      RequestReviewFromNone -> "none"
      RequestReviewFromAuthor -> "author"
      RequestReviewFromOwner -> "owner"
      RequestReviewFrom name -> name

readRequestReviewFrom :: Text -> RequestReviewFrom
readRequestReviewFrom = \case
  "none" -> RequestReviewFromNone
  "author" -> RequestReviewFromAuthor
  "owner" -> RequestReviewFromOwner
  x -> RequestReviewFrom x

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
