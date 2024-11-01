-- |
--
-- Module      : Restyler.Config.ImageCleanup
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.ImageCleanup
  ( HasImageCleanup (..)
  , imageCleanupParser
  ) where

import Restyler.Prelude

import OptEnvConf

class HasImageCleanup env where
  getImageCleanup :: env -> Bool

imageCleanupParser :: Parser Bool
imageCleanupParser =
  withDefault False
    $ yesNoSwitch
      [ help "Remove images after running them"
      , long "image-cleanup"
      , env "IMAGE_CLEANUP"
      , conf "image_cleanup"
      ]
