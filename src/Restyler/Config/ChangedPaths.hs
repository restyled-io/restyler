module Restyler.Config.ChangedPaths
    ( ChangedPathsConfig(..)
    , MaximumChangedPathsOutcome(..)
    )
where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Casing
import Restyler.Config.ExpectedKeys

data ChangedPathsConfig = ChangedPathsConfig
    { cpcMaximum :: Natural
    , cpcOutcome :: MaximumChangedPathsOutcome
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON ChangedPathsConfig where
    parseJSON = genericParseJSONValidated $ aesonPrefix snakeCase

instance ToJSON ChangedPathsConfig where
    toJSON = genericToJSON $ aesonPrefix snakeCase
    toEncoding = genericToEncoding $ aesonPrefix snakeCase

data MaximumChangedPathsOutcome
    = MaximumChangedPathsOutcomeSkip
    | MaximumChangedPathsOutcomeError
    deriving stock (Eq, Show)

instance FromJSON MaximumChangedPathsOutcome where
    parseJSON = withText "MaximumChangedPathsOutcome" $ \case
        "skip" -> pure MaximumChangedPathsOutcomeSkip
        "error" -> pure MaximumChangedPathsOutcomeError
        x -> fail $ "Invalid outcome " <> unpack x <> ", must be skip or error"

instance ToJSON MaximumChangedPathsOutcome where
    toJSON = \case
        MaximumChangedPathsOutcomeSkip -> String "skip"
        MaximumChangedPathsOutcomeError -> String "error"
