-- |
--
-- Module      : Restyler.Config.RestylerSpec
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.RestylerSpec
  ( spec
  ) where

import Restyler.Prelude

import Autodocodec (HasCodec)
import Autodocodec.Yaml (eitherDecodeYamlViaCodec)
import Data.Aeson (FromJSON)
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import Restyler.Config.Restyler
import Restyler.Restyler
import Restyler.Test.App
import Restyler.Test.Fixtures

spec :: Spec
spec = do
  withSimpleTestApp $ do
    describe "autoEnableOverrides" $ do
      let
        mkRestylersYaml :: [[Text]] -> [Text]
        mkRestylersYaml =
          concatMap
            ( <>
                [ "  image: restyled/x:dev"
                , "  command: []"
                , "  arguments: []"
                , "  include: []"
                , "  interpreters: []"
                , "  documentation: []"
                ]
            )

      context "haskell example" $ do
        let
          restylersYaml :: [Text]
          restylersYaml =
            mkRestylersYaml
              [
                [ "- name: fourmolu"
                , "  auto_enable:"
                , "    config_patterns: ['.fourmolu.yaml']"
                , "    group: {name: 'haskell', priority: 5}"
                ]
              ,
                [ "- name: brittany"
                , "  auto_enable:"
                , "    config_patterns: ['.brittany.yaml']"
                , "    group: {name: 'haskell', priority: 10}"
                ]
              ,
                [ "- name: stylish-haskell"
                , "  auto_enable:"
                , "    config_patterns: ['.stylish-haskell.yaml']"
                , "    group: {name: 'haskell', priority: 0}"
                ]
              ]

        it "by priority" $ do
          overrides <- autoEnableTest restylersYaml Nothing
          overrides `shouldBe` [enabled "brittany"]

        it "by configuration" $ do
          writeFile ".fourmolu.yaml" ""

          overrides <- autoEnableTest restylersYaml Nothing
          overrides `shouldBe` [enabled "fourmolu"]

        it "multiple configurations" $ do
          writeFile ".fourmolu.yaml" ""
          writeFile ".brittany.yaml" ""

          overrides <- autoEnableTest restylersYaml Nothing
          overrides `shouldBe` [enabled "fourmolu", enabled "brittany"]

        it "explicit selection" $ do
          overrides <- autoEnableTest restylersYaml $ Just ["- stylish-haskell"]
          overrides `shouldBe` [enabled "stylish-haskell"]

        it "explicit selection with config" $ do
          writeFile ".brittany.yaml" ""
          overrides <- autoEnableTest restylersYaml $ Just ["- stylish-haskell"]
          overrides `shouldBe` [enabled "brittany", enabled "stylish-haskell"]

      context "json example" $ do
        let
          restylersYaml :: [Text]
          restylersYaml =
            mkRestylersYaml
              [
                [ "- name: jq"
                , "  auto_enable:"
                , "    group: {name: 'json', priority: 0}"
                ]
              ,
                [ "- name: prettier-json"
                , "  auto_enable:"
                , "    group: {name: 'json', priority: 5}"
                ]
              ]
        it "by priority" $ do
          overrides <- autoEnableTest restylersYaml Nothing
          overrides `shouldBe` [enabled "prettier-json"]

        it "explicit selection" $ do
          overrides <- autoEnableTest restylersYaml $ Just ["- jq"]
          overrides `shouldBe` [enabled "jq"]

  describe "overrideRestylers" $ do
    it "fails if wildcard is specified more than once" $ do
      let result =
            overrideRestylers
              []
              [ enabled "fourmolu"
              , wildcard
              , disabled "stylish-haskell"
              , wildcard
              ]

      result `shouldHaveFailure` "at most 1 wildcard"
      result `shouldHaveFailure` "2 found"

    it "fails for an unknown name" $ do
      let result =
            overrideRestylers
              [ someRestyler "brittany"
              , someRestyler "fourmolu"
              , someRestyler "hlint"
              , someRestyler "stylish-haskell"
              ]
              [ enabled "fourmolu"
              , disabled "stylish-haskell"
              , enabled "boppity-boopy"
              ]

      result `shouldHaveFailure` "Unexpected"
      result `shouldHaveFailure` "name boppity-boopy"

    it "places the remaining restylers where the wildcard was" $ do
      let
        restylers =
          [ someRestyler "brittany"
          , someRestyler "fourmolu"
          , someRestyler "hlint"
          , someRestyler "stylish-haskell"
          ]
        resultNone =
          overrideRestylers
            restylers
            [ enabled "fourmolu"
            , disabled "stylish-haskell"
            ]
        resultFirst =
          overrideRestylers
            restylers
            [ wildcard
            , enabled "fourmolu"
            , disabled "stylish-haskell"
            ]
        resultLast =
          overrideRestylers
            restylers
            [ enabled "fourmolu"
            , disabled "stylish-haskell"
            , wildcard
            ]

      resultNone
        `shouldBe` Right
          [ someRestyler "fourmolu"
          , (someRestyler "stylish-haskell") {rEnabled = False}
          ]
      resultFirst
        `shouldBe` Right
          [ someRestyler "brittany"
          , someRestyler "hlint"
          , someRestyler "fourmolu"
          , (someRestyler "stylish-haskell") {rEnabled = False}
          ]
      resultLast
        `shouldBe` Right
          [ someRestyler "fourmolu"
          , (someRestyler "stylish-haskell") {rEnabled = False}
          , someRestyler "brittany"
          , someRestyler "hlint"
          ]

autoEnableTest
  :: (MonadDirectory m, MonadIO m, MonadLogger m)
  => [Text]
  -- ^ restylers.yaml contents
  -> Maybe [Text]
  -- ^ .restyled.yaml contents
  -> m [RestylerOverride]
autoEnableTest restylersYaml mRestyledYaml = do
  restylers <- yamlFromLinesJSON restylersYaml
  overrides <- maybe (pure []) yamlFromLines mRestyledYaml
  autoEnableOverrides restylers overrides

yamlFromLines :: (HasCodec a, MonadIO m) => [Text] -> m a
yamlFromLines = yamlFromLines' eitherDecodeYamlViaCodec

yamlFromLinesJSON :: (FromJSON a, MonadIO m) => [Text] -> m a
yamlFromLinesJSON = yamlFromLines' Yaml.decodeEither'

yamlFromLines'
  :: MonadIO m
  => (ByteString -> Either Yaml.ParseException a)
  -> [Text]
  -> m a
yamlFromLines' f ts = case f $ encodeUtf8 $ T.unlines ts of
  Left err -> do
    liftIO
      $ expectationFailure
      $ "Failed to parse YAML: "
      <> Yaml.prettyPrintParseException err
    error "unreachable"
  Right a -> pure a

shouldHaveFailure :: (HasCallStack, Show a) => Either [Text] a -> Text -> IO ()
shouldHaveFailure result needle = go result
 where
  go = \case
    Left (e : es)
      | needle `T.isInfixOf` e -> pure ()
      | otherwise -> go $ Left es
    _ ->
      expectationFailure
        $ "Expected: Left containing "
        <> show needle
        <> "."
        <> "\nActual: "
        <> show result

infix 1 `shouldHaveFailure`
