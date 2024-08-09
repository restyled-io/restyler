-- |
--
-- Module      : Restyler.Yaml.Errata
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Yaml.Errata
  ( formatInvalidYaml
  ) where

import Restyler.Prelude

import Data.ByteString.Char8 qualified as BS8
import Data.Text.Lazy qualified as TL
import Data.Yaml (YamlMark (..))
import Errata
import Errata.Styles
import Errata.Types (Column, Line)

formatInvalidYaml
  :: FilePath -> ByteString -> String -> String -> YamlMark -> Text
formatInvalidYaml path src problem context mark =
  TL.toStrict
    $ prettyErrors (decodeUtf8 @Text src)
    $ fromMaybe (yamlErrata path problem context mark)
    $ do
      guard $ problem == tabProblem && context == tabContext
      c <- getCharAt (yamlLine nextMark) (yamlColumn nextMark) src
      guard $ c == '\t'
      pure
        $ yamlErrata
          path
          "Tab found for indentation"
          "YAML forbids tabs for indentation: https://yaml.org/faq.html"
          nextMark
 where
  nextMark =
    mark
      { yamlLine = yamlLine mark + 1
      , yamlColumn = 1
      }

yamlErrata :: FilePath -> String -> String -> YamlMark -> [Errata]
yamlErrata path problem context mark =
  [ errataSimple
      (Just "Yaml parse exception")
      ( blockSimple
          basicStyle
          basicPointer
          path
          Nothing
          ( yamlLine mark
          , yamlColumn mark
          , yamlColumn mark + 1
          , Just $ pack problem
          )
          (Just $ pack context)
      )
      Nothing
  ]

tabProblem :: String
tabProblem = "found character that cannot start any token"

tabContext :: String
tabContext = "while scanning for the next token"

-- | Return 'Char' at (1-based) line/column of a 'ByteString'
getCharAt :: Line -> Column -> ByteString -> Maybe Char
getCharAt ln cn bs = do
  l <- (!? (ln - 1)) $ BS8.lines bs
  BS8.indexMaybe l (cn - 1)
