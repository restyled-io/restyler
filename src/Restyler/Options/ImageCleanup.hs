-- |
--
-- Module      : Restyler.Options.ImageCleanup
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options.ImageCleanup
  ( ImageCleanupOption (..)
  , HasImageCleanupOption (..)
  , getImageCleanup
  , envImageCleanupOption
  , optImageCleanupOption
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative

newtype ImageCleanupOption = ImageCleanupOption
  { unwrap :: Any
  }
  deriving newtype (Semigroup, Monoid)

class HasImageCleanupOption a where
  getImageCleanupOption :: a -> ImageCleanupOption

getImageCleanup :: (MonadReader env m, HasImageCleanupOption env) => m Bool
getImageCleanup = asks $ getAny . (.unwrap) . getImageCleanupOption

envImageCleanupOption :: Env.Parser Env.Error ImageCleanupOption
envImageCleanupOption =
  ImageCleanupOption . Any <$> Env.switch "IMAGE_CLEANUP" (Env.help optionHelp)

optImageCleanupOption :: Parser ImageCleanupOption
optImageCleanupOption =
  ImageCleanupOption . Any <$> switch (long "image-cleanup" <> help optionHelp)

optionHelp :: String
optionHelp = "Remove pulled restyler images after restyling"
