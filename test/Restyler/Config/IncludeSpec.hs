-- |
--
-- Module      : Restyler.Config.IncludeSpec
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config.IncludeSpec
  ( spec
  )
where

import Restyler.Prelude

import Restyler.Config.Include
import Test.Hspec

spec :: Spec
spec = do
  describe "includePath" $ do
    it "does not match if empty" $ do
      includePath [] "anything" `shouldBe` False

    it "matches if any includes" $ do
      let includes = [Include "**/*.py", Include "foo/*"]

      includePath includes "foo.py" `shouldBe` True
      includePath includes "foo/bar.py" `shouldBe` True
      includePath includes "foo/baz.hs" `shouldBe` True

    it "does not match if the path is negated after" $ do
      let includes = [Include "foo/*", Negated "**/*.pyc"]

      includePath includes "foo/bar.py" `shouldBe` True
      includePath includes "foo/bar.pyc" `shouldBe` False

    it "matches if the path is re-included later" $ do
      let includes = [Negated "foo/*", Include "**/*.py"]

      includePath includes "foo/bar.py" `shouldBe` True
      includePath includes "foo/bar.pyc" `shouldBe` False

    it "matches hidden files, if you know how" $ do
      let includes =
            [ Include "**/*.json"
            , Include ".**/*.json"
            , Include ".**/.*.json"
            , Include "**/.*.json"
            ]

      includePath includes "foo/bar.json" `shouldBe` True
      includePath includes ".foo/bar.json" `shouldBe` True
      includePath includes "foo/.bar.json" `shouldBe` True
      includePath includes ".foo/.bar.json" `shouldBe` True
