module Restyler.GitHub.Repository
  ( Repository (..)
  ) where

import Restyler.Prelude

data Repository = Repository
  { owner :: Text
  , repo :: Text
  }
  deriving stock (Eq, Show)
