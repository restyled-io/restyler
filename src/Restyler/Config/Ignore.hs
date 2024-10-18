module Restyler.Config.Ignore
  ( HasIgnores (..)
  , Ignores (..)
  , ignoresParser
  ) where

import Restyler.Prelude hiding ((.=))

import Data.Semigroup.Generic
import OptEnvConf
import Restyler.Config.Glob

class HasIgnores env where
  getIgnores :: env -> Ignores

data Ignores = Ignores
  { byAuthor :: [Glob Text]
  , byBranch :: [Glob Text]
  , byLabels :: [Glob Text]
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid Ignores

ignoresParser :: Parser Ignores
ignoresParser = mconcat <$> sequenceA (newIgnoresParser : oldIgnoresParsers)

newIgnoresParser :: Parser Ignores
newIgnoresParser =
  subAll "ignore"
    $ Ignores
    <$> setting
      [ help "Ignore authors that match globs"
      , option
      , long "authors"
      , reader $ commaSeparatedList str
      , metavar "GLOB[,GLOB...]"
      , conf "authors"
      ]
    <*> setting
      [ help "Ignore branches that match globs"
      , option
      , long "branches"
      , reader $ commaSeparatedList str
      , metavar "GLOB[,GLOB...]"
      , conf "branches"
      ]
    <*> setting
      [ help "Ignore labels that match globs"
      , option
      , long "labels"
      , reader $ commaSeparatedList str
      , metavar "GLOB[,GLOB...]"
      , conf "labels"
      ]

oldIgnoresParsers :: [Parser Ignores]
oldIgnoresParsers =
  [ Ignores
      <$> setting
        [ help ""
        , conf "ignore_authors"
        , hidden
        ]
      <*> pure []
      <*> pure []
  , Ignores
      <$> setting
        [ help ""
        , conf "ignore_branches"
        , hidden
        ]
      <*> pure []
      <*> pure []
  , Ignores
      <$> setting
        [ help ""
        , conf "ignore_labels"
        , hidden
        ]
      <*> pure []
      <*> pure []
  ]
