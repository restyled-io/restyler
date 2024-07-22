module Restyler.Job.RepoDisabled
  ( RepoDisabled (..)
  ) where

import Restyler.Prelude

data RepoDisabled = RepoDisabled
  deriving stock (Show)

instance Exception RepoDisabled where
  displayException _ =
    "This repository has been disabled for possible abuse."
      <> " If you believe this is an error, please reach out to"
      <> " support@restyled.io"
