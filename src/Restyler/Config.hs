{-# LANGUAGE FieldSelectors #-}

-- | Handling of @.restyled.yaml@ content and behavior driven there-by
--
-- Module      : Restyler.Config
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config
  ( Config (..)
  , loadConfig

    -- * New stuff
  , module Restyler.Config.Parse
  ) where

import Restyler.Prelude

import Restyler.Config.Glob
import Restyler.Config.Parse

-- | Fully resolved configuration
--
-- This is what we work with throughout the system.
data Config = Config
  { cIgnoreAuthors :: [Glob Text]
  , cIgnoreBranches :: [Glob Text]
  , cIgnoreLabels :: [Glob Text]
  }

loadConfig :: Applicative f => f Config
loadConfig = pure $ Config [] [] []
