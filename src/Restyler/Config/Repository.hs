-- |
--
-- Module      : Restyler.Config.Repository
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.Repository
  ( RepositoryOption (..)
  ) where

import Restyler.Prelude

data RepositoryOption = RepositoryOption
  { owner :: Text
  , repo :: Text
  }
