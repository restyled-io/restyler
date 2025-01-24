{-# LANGUAGE DuplicateRecordFields #-}

-- |
--
-- Module      : Restyler.Config.Ignore
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.Ignore
  ( HasIgnores (..)
  , Ignores (..)
  , ignoresParser
  ) where

import Restyler.Prelude hiding ((.=))

import OptEnvConf
import Restyler.Config.Glob

class HasIgnores env where
  getIgnores :: env -> Ignores

data Ignores = Ignores
  { byAuthor :: [Glob Text]
  , byBranch :: [Glob Text]
  , byLabels :: [Glob Text]
  }
  deriving stock (Eq, Show)

ignoresParser :: Parser Ignores
ignoresParser =
  go
    <$> newIgnoresParser
    <*> oldIgnoresParser
 where
  go new old
    | old /= OldIgnores Nothing Nothing Nothing =
        Ignores
          { byAuthor = fromMaybe new.byAuthor old.byAuthor
          , byBranch = fromMaybe new.byBranch old.byBranch
          , byLabels = fromMaybe new.byLabels old.byLabels
          }
    | otherwise = new

newIgnoresParser :: Parser Ignores
newIgnoresParser =
  subAll "ignore"
    $ Ignores
    <$> setting
      [ help "Ignore authors that match globs"
      , option
      , name "authors"
      , reader $ commaSeparatedList str
      , metavar "GLOB[,GLOB]"
      , value ["*[bot]"]
      ]
    <*> setting
      [ help "Ignore branches that match globs"
      , option
      , name "branches"
      , reader $ commaSeparatedList str
      , metavar "GLOB[,GLOB]"
      , value ["renovate/*"]
      ]
    <*> setting
      [ help "Ignore labels that match globs"
      , option
      , name "labels"
      , reader $ commaSeparatedList str
      , metavar "GLOB[,GLOB]"
      , value ["restyled-ignore"]
      ]

data OldIgnores = OldIgnores
  { byAuthor :: Maybe [Glob Text]
  , byBranch :: Maybe [Glob Text]
  , byLabels :: Maybe [Glob Text]
  }
  deriving stock (Eq)

oldIgnoresParser :: Parser OldIgnores
oldIgnoresParser =
  OldIgnores
    <$> optional (hiddenIgnoreParser "ignore_authors")
    <*> optional (hiddenIgnoreParser "ignore_branches")
    <*> optional (hiddenIgnoreParser "ignore_labels")

hiddenIgnoreParser :: String -> Parser [Glob Text]
hiddenIgnoreParser key =
  setting
    [ help ""
    , conf key
    , hidden
    ]
