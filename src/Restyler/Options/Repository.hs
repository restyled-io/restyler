module Restyler.Options.Repository
  ( RepositoryOption (..)
  ) where

import Restyler.Prelude

data RepositoryOption = RepositoryOption
  { owner :: Text
  , repo :: Text
  }
  deriving stock (Eq, Show)
