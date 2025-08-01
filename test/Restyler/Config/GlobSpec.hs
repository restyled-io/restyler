module Restyler.Config.GlobSpec
  ( spec
  ) where

import Restyler.Prelude

import Restyler.Config.Glob
import Restyler.Test.App

spec :: Spec
spec = withSimpleTestApp $ do
  describe "matchAnyInDirectory" $ do
    let
      globs :: [Glob FilePath]
      globs =
        [ ".fourmolu.yaml"
        , ".fourmolu/*.yaml"
        , "config/fourmolu/**/*.yaml"
        ]

    it "no matches" $ do
      matchAnyInCurrentDirectory globs `shouldReturn` False

    it "top-level" $ do
      writeFile ".fourmolu.yaml" ""
      matchAnyInCurrentDirectory globs `shouldReturn` True

    it "sub-directory" $ do
      writeFile ".fourmolu/config.yaml" ""
      matchAnyInCurrentDirectory globs `shouldReturn` True

    it "extended glob" $ do
      writeFile "config/fourmolu/x/y.yaml" ""
      matchAnyInCurrentDirectory globs `shouldReturn` True
