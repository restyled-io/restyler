{-# LANGUAGE NoFieldSelectors #-}

module Restyler.GitHub.Repository
  ( Repository (..)
  ) where

import Restyler.Prelude

import Data.Aeson (ToJSON)

data Repository = Repository
  { owner :: Text
  , repo :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
