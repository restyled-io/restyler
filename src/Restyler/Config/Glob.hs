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

    -- * /match/ functions return boolean
  , match
  , matchPath
  , matchAny
  , matchAnyInCurrentDirectory

    -- * /glob/ functions return matched items
  , globFirst
  , globAnyInCurrentDirectory
  ) where

import Restyler.Prelude

import Autodocodec (Autodocodec (..), HasCodec)
import Data.Aeson
import Restyler.Monad.Directory
import System.FilePath.Glob hiding (match)

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
match (Glob p) =
  matchWith
    (matchDefault {matchDotsImplicitly = True})
    (compileWith (getCompOptions @a) p)
    . forMatch

matchPath :: Glob FilePath -> Path b t -> Bool
matchPath (Glob p) =
  matchWith
    (matchDefault {matchDotsImplicitly = True})
    (compileWith (getCompOptions @FilePath) p)
    . forMatch
    . toFilePath

matchAny :: (Foldable t, GlobTarget a) => [Glob a] -> t a -> Bool
matchAny gs = any $ \x -> any (`match` x) gs

matchAnyInCurrentDirectory :: MonadDirectory m => [Glob FilePath] -> m Bool
matchAnyInCurrentDirectory gs = do
  files <- listDirectoryRecur =<< getCurrentDirectory
  pure $ matchAny gs $ map toFilePath files

globFirst :: (Foldable t, GlobTarget a) => [Glob a] -> t a -> Maybe a
globFirst gs = find $ \x -> any (`match` x) gs

globAnyInCurrentDirectory
  :: MonadDirectory m => [Glob FilePath] -> m [Path Rel File]
globAnyInCurrentDirectory gs = do
  files <- listDirectoryRecur =<< getCurrentDirectory
  pure $ filter (\f -> any (`match` toFilePath f) gs) files
