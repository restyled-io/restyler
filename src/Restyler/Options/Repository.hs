module Restyler.Options.Repository
  ( RepositoryOption (..)
  ) where

import Restyler.Prelude

import Data.Aeson (ToJSON)

data RepositoryOption = RepositoryOption
  { owner :: Text
  , repo :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
