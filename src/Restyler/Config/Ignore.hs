module Restyler.Config.Ignore
  ( HasIgnores (..)
  , Ignores (..)
  , ignoresParser
  ) where

import Restyler.Prelude hiding ((.=))

import Autodocodec
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

instance HasCodec Ignores where
  codec =
    object "Ignores"
      $ Ignores
      <$> (requiredField "author" "Author globs to ignore" .= (.byAuthor))
      <*> (requiredField "branch" "Branch globs to ignore" .= (.byBranch))
      <*> (requiredField "labels" "Labels globs to ignore" .= (.byLabels))

ignoresParser :: Parser Ignores
ignoresParser =
  mconcat
    <$> sequenceA
      [ setting
          [ help "Ignore by author, branch, or labels"
          , conf "ignore"
          ]
      , ignoreAuthorParser
      , ignoreBranchParser
      , ignoreLabelsParser
      ]

ignoreAuthorParser :: Parser Ignores
ignoreAuthorParser = pure mempty -- TODO

ignoreBranchParser :: Parser Ignores
ignoreBranchParser = pure mempty -- TODO

ignoreLabelsParser :: Parser Ignores
ignoreLabelsParser = pure mempty -- TODO
