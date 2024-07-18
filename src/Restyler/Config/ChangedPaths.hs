module Restyler.Config.ChangedPaths
  ( ChangedPathsConfig (..)
  , MaximumChangedPathsOutcome (..)
  ) where

import Restyler.Prelude

import Data.Aeson

data ChangedPathsConfig = ChangedPathsConfig
  { maximum :: Natural
  , outcome :: MaximumChangedPathsOutcome
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

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
