{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- |
--
-- Module      : Restyler.Yaml.ErrataSpec
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Yaml.ErrataSpec
  ( spec
  ) where

import SpecHelper

import Data.Aeson
import Data.Text qualified as T
import Data.Yaml (ParseException (..), YamlException (..))
import Data.Yaml qualified as Yaml
import Restyler.Yaml.Errata

spec :: Spec
spec = do
  describe "formatInvalidYaml" $ do
    it "adds an annotation pointing out line/column" $ example $ do
      let
        invalidYaml :: ByteString
        invalidYaml =
          encodeUtf8
            $ unlines
              [ "restylers:"
              , "  - brittany"
              , "      include:"
              , "        - '**/*.hs'"
              , "        - '!src/Graphula/Class.hs' # CPP"
              , "  - stylish-haskell"
              , ""
              , "comments: false"
              , ""
              , "labels:"
              , "  - restyled"
              ]

        Left (InvalidYaml (Just (YamlParseException p c m))) =
          Yaml.decodeEither' @Value invalidYaml

      formatInvalidYaml "<config>" invalidYaml p c m
        `shouldBeLines` [ "Yaml parse exception"
                        , "--> <config>:2:13"
                        , "  |"
                        , "2 |   - brittany"
                        , "  |             ^ mapping values are not allowed in this context"
                        ]

    it "handles tabs nicely" $ example $ do
      let
        invalidYaml :: ByteString
        invalidYaml =
          encodeUtf8
            $ unlines
              [ "statuses:"
              , "\tdifferences: false"
              ]

        Left (InvalidYaml (Just (YamlParseException p c m))) =
          Yaml.decodeEither' @Value invalidYaml

      formatInvalidYaml "<config>" invalidYaml p c m
        `shouldBeLines` [ "Yaml parse exception"
                        , "--> <config>:2:1"
                        , "  |"
                        , "2 |     differences: false"
                        , "  | ^^^^ Tab found for indentation"
                        , "YAML forbids tabs for indentation: https://yaml.org/faq.html"
                        ]

shouldBeLines :: MonadIO m => Text -> [Text] -> m ()
shouldBeLines x = shouldBe (T.dropWhileEnd (== '\n') x <> "\n") . T.unlines

infix 1 `shouldBeLines`
