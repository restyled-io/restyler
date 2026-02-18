{-# OPTIONS_GHC -Wno-orphans #-}

-- |
--
-- Module      : Restyler.OrphanInstances
-- Copyright   : (c) 2026 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.OrphanInstances () where

import Prelude

import Autodocodec
import Data.Bifunctor (first)
import Path

instance HasCodec (Path Rel File) where
  codec = bimapCodec (first show . parseRelFile) toFilePath stringCodec
