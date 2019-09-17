{-# LANGUAGE LambdaCase #-}

module Restyler.Config.RequestReview
    ( RequestReviewConfig
    , determineReviewer
    )
where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Types (typeMismatch)
import Data.Bool (bool)
import GitHub.Data (User)
import Restyler.Config.ExpectedKeys
import Restyler.PullRequest

data RequestReviewFrom
    = RequestReviewFromNone
    | RequestReviewFromAuthor
    | RequestReviewFromOwner
    deriving (Eq, Show, Generic)

instance FromJSON RequestReviewFrom where
    parseJSON =
        withText "RequestReviewFrom" $ either fail pure . readRequestReviewFrom

instance ToJSON RequestReviewFrom where
    toJSON RequestReviewFromNone = String "none"
    toJSON RequestReviewFromAuthor = String "author"
    toJSON RequestReviewFromOwner = String "owner"

readRequestReviewFrom :: Text -> Either String RequestReviewFrom
readRequestReviewFrom = \case
    "none" -> Right RequestReviewFromNone
    "author" -> Right RequestReviewFromAuthor
    "owner" -> Right RequestReviewFromOwner
    x -> Left $ mconcat
        [ "Invalid RequestReviewFrom value: " <> show x
        , "\n  Valid values: none, author, or owner."
        ]

data RequestReviewConfig = RequestReviewConfig
    { rrcOrigin :: RequestReviewFrom
    , rrcForked :: RequestReviewFrom
    }
    deriving (Eq, Show, Generic)

bothFrom :: RequestReviewFrom -> RequestReviewConfig
bothFrom x = RequestReviewConfig { rrcOrigin = x, rrcForked = x }

-- brittany-disable-next-binding

instance FromJSON RequestReviewConfig where
    parseJSON (String t) =
        either fail (pure . bothFrom) $ readRequestReviewFrom t
    parseJSON (Object o) = do
        validateObjectKeys ["origin", "forked"] o
        RequestReviewConfig
            <$> o .:? "origin" .!= RequestReviewFromAuthor
            <*> o .:? "forked" .!= RequestReviewFromOwner
    parseJSON x = typeMismatch
        "Invalid type for RequestReview. Expected String or Object."
        x

instance ToJSON RequestReviewConfig where
    toJSON = genericToJSON $ aesonPrefix snakeCase
    toEncoding = genericToEncoding $ aesonPrefix snakeCase

determineReviewer
    :: PullRequest -- ^ The Original PR
    -> RequestReviewConfig
    -> Maybe (Name User)
determineReviewer pr RequestReviewConfig {..} =
    (`reviewerFor` pr) $ bool rrcOrigin rrcForked $ pullRequestIsFork pr

reviewerFor :: RequestReviewFrom -> PullRequest -> Maybe (Name User)
reviewerFor RequestReviewFromNone = const Nothing
reviewerFor RequestReviewFromAuthor = Just . pullRequestUserLogin
reviewerFor RequestReviewFromOwner = Just . coerceName . pullRequestOwnerName

-- TODO: centralize this?
coerceName :: Name a -> Name b
coerceName = mkName Proxy . untagName
