{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Config.RequestReview
    ( RequestReviewConfig
    , determineReviewer
    )
where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Bool (bool)
import Restyler.Config.ExpectedKeys
import Restyler.PullRequest

data RequestReviewFrom
    = RequestReviewFromAuthor
    -- ^ Request review from the Author of the original PR
    | RequestReviewFromOwner
    -- ^ Request review from the Owner of the original PR
    deriving (Eq, Show, Generic)

instance FromJSON RequestReviewFrom where
    parseJSON =
        withText "RequestReviewFrom" $ either fail pure . readRequestReviewFrom

instance ToJSON RequestReviewFrom where
    toJSON RequestReviewFromAuthor = String "author"
    toJSON RequestReviewFromOwner = String "owner"

readRequestReviewFrom :: Text -> Either String RequestReviewFrom
readRequestReviewFrom = \case
    "author" -> Right RequestReviewFromAuthor
    "owner" -> Right RequestReviewFromOwner
    x -> Left $ mconcat
        [ "Invalid RequestReviewFrom value: " <> show x
        , "\n  Valid values: author, owner."
        ]

data RequestReviewConfig = RequestReviewConfig
    { rrcOrigin :: RequestReviewFrom
    , rrcForked :: RequestReviewFrom
    }
    deriving (Eq, Show, Generic)

instance FromJSON RequestReviewConfig where
    -- | Parse a single string or object configuration
    --
    -- @'String'@ requests review from that value in all cases:
    --
    -- @
    -- ---
    -- request_review: author
    -- @
    --
    -- Use an @'Object'@ to specify the reviewer for origin and forked cases
    -- separately. Either can be omitted and will inherit the default shown:
    --
    -- @
    -- ---
    -- request_review:
    --   origin: author
    --   forked: owner
    -- @
    --
    parseJSON (String t) =
        either fail (pure . simpleRequestReviewConfig)
            $ readRequestReviewFrom t

    parseJSON (Object o) = do
        validateObjectKeys ["origin", "forked"] o
        RequestReviewConfig
            <$> o .:? "origin" .!= rrcOrigin defaultRequestReviewConfig
            <*> o .:? "forked" .!= rrcForked defaultRequestReviewConfig

    parseJSON x = typeMismatch
        "Invalid type for RequestReview. Expected String or Object."
        x

instance ToJSON RequestReviewConfig where
    toJSON RequestReviewConfig{..} = object
        [ "origin" .= rrcOrigin
        , "forked" .= rrcForked
        ]

-- | Given a Config and PR, determine who should be requested as reviewer
determineReviewer
    :: RequestReviewConfig
    -> PullRequest -- ^ The Original PR
    -> Name User
determineReviewer RequestReviewConfig {..} pr =
    (`reviewerFor` pr) $ bool rrcOrigin rrcForked $ pullRequestIsFork pr

reviewerFor :: RequestReviewFrom -> PullRequest -> Name User
reviewerFor RequestReviewFromAuthor = pullRequestUserLogin
reviewerFor RequestReviewFromOwner = coerceName . pullRequestOwnerName

defaultRequestReviewConfig :: RequestReviewConfig
defaultRequestReviewConfig = RequestReviewConfig
    { rrcOrigin = RequestReviewFromAuthor
    , rrcForked = RequestReviewFromOwner
    }

simpleRequestReviewConfig :: RequestReviewFrom -> RequestReviewConfig
simpleRequestReviewConfig x =
    RequestReviewConfig {rrcOrigin = x, rrcForked = x}

-- TODO: centralize this?
coerceName :: Name a -> Name b
coerceName = mkName Proxy . untagName
