-- |
--
-- Module      : Restyler.Config.Include
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.Include
  ( Include (..)
  , explicit
  , includePath
  ) where

import Restyler.Prelude

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import System.FilePath.Glob
  ( MatchOptions (..)
  , Pattern
  , compile
  , decompile
  , matchDefault
  , matchWith
  )

data Include
  = -- | @**\/*.hs@
    Include Pattern
  | -- | @!**\/*.temp@
    Negated Pattern
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec Include)

instance IsString Include where
  fromString = includeFromText . pack

instance HasCodec Include where
  codec =
    bimapCodec (Right . includeFromText) includeToText textCodec
      <?> "<pattern>|!<pattern>"

includeFromText :: Text -> Include
includeFromText = go . unpack
 where
  go = \case
    '!' : rest -> Negated $ compile rest
    x -> Include $ compile x

includeToText :: Include -> Text
includeToText = \case
  Include p -> pack $ decompile p
  Negated p -> pack $ "!" <> decompile p

-- | Build an @'Include'@ matching a path exactly
explicit :: FilePath -> Include
explicit = Include . compile

-- | Determine if a set of @'Include'@s match a file
--
-- Don't try to over-think this. It works how you would expect, and you can
-- confirm in its test cases.
includePath :: [Include] -> FilePath -> Bool
includePath is fp = foldl' go False is
 where
  go :: Bool -> Include -> Bool
  go b (Include p) = b || p `match` fp
  go b (Negated p) = b && not (p `match` fp)

match :: Pattern -> FilePath -> Bool
match = matchWith $ matchDefault {matchDotsImplicitly = True}
