-- |
--
-- Module      : Restyler.Options.ImageCleanup
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options.ImageCleanup
  ( HasImageCleanup (..)
  , imageCleanupParser
  ) where

import Restyler.Prelude

import OptEnvConf

class HasImageCleanup env where
  getImageCleanup :: env -> Bool

imageCleanupParser :: Parser Bool
imageCleanupParser =
  yesNoSwitch
    [ help "Remove pulled restyler images after restyling"
    , long "image-cleanup"
    , env "IMAGE_CLEANUP"
    , conf "image_cleanup"
    , value False
    ]
