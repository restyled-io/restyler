{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Small wrapper over @'System.FilePath.Glob.Pattern'@
--
-- Module      : Restyler.Config.Glob
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.Glob
  ( Glob (..)
  , GlobTarget (..)
  , match
  , matchAny
  , matchFirst
  , matchAnyInDirectory
  ) where

import Restyler.Prelude

import Autodocodec (Autodocodec (..), HasCodec)
import Data.Aeson
import Restyler.Monad.Directory
import System.FilePath ((</>))
import System.FilePath.Glob hiding (match)
import System.FilePath.Glob qualified as Glob

newtype Glob a = Glob {unwrap :: String}
  deriving stock (Eq)
  deriving newtype (HasCodec, IsString, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec (Glob a))

class GlobTarget a where
  forMatch :: a -> String
  getCompOptions :: CompOptions

instance GlobTarget FilePath where
  forMatch = id
  getCompOptions = compDefault

instance GlobTarget Text where
  forMatch = unpack
  getCompOptions =
    compDefault
      { characterClasses = False
      , characterRanges = False
      , numberRanges = False
      }

match :: forall a. GlobTarget a => Glob a -> a -> Bool
match (Glob p) = Glob.match (compileWith (getCompOptions @a) p) . forMatch

matchAny :: (Foldable t, GlobTarget a) => [Glob a] -> t a -> Bool
matchAny globs = any $ \x -> any (`match` x) globs

matchFirst :: (Foldable t, GlobTarget a) => [Glob a] -> t a -> Maybe a
matchFirst globs = find $ \x -> any (`match` x) globs

matchAnyInDirectory :: MonadDirectory m => [Glob FilePath] -> FilePath -> m Bool
matchAnyInDirectory gs d = do
  -- If listing the current directory, don't prefix the results
  prefix <- bool (d </>) id . (== d) <$> getCurrentDirectory
  contents <- map prefix <$> listDirectory d
  subdirs <- filterM doesDirectoryExist contents

  if matchAny gs contents
    then pure True
    else anyM (matchAnyInDirectory gs) subdirs
