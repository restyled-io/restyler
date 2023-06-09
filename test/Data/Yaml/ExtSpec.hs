{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Data.Yaml.ExtSpec
  ( spec
  ) where

import SpecHelper

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Yaml.Ext

spec :: Spec
spec = do
  describe "locateErrorInContent" $ do
    it "adds an annotation pointing out line/column" $ example $ do
      let
        invalidYaml =
          T.unlines
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

        Left ex = Yaml.decodeEither' @Value $ encodeUtf8 invalidYaml

      locateErrorInContent ex invalidYaml
        `shouldBe` T.unlines
          [ "restylers:"
          , "  - brittany"
          , "            ▲"
          , "            │"
          , "            └ mapping values are not allowed in this context"
          , ""
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
