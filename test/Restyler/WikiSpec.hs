module Restyler.WikiSpec
  ( spec
  ) where

import Restyler.Prelude

import Restyler.Wiki qualified as Wiki
import Test.Hspec

spec :: Spec
spec = do
  describe "commonEror" $ do
    it "builds a Common Errors page" $ do
      Wiki.commonError "Plan Upgrade Required"
        `shouldBe` "https://github.com/restyled-io/restyled.io/wiki/Common-Errors:-Plan-Upgrade-Required"

  describe "page" $ do
    it "builds a page spelled out exactly" $ do
      Wiki.page "/Disabling-Restyled"
        `shouldBe` "https://github.com/restyled-io/restyled.io/wiki/Disabling-Restyled"

    it "auto-slash-prefixes" $ do
      Wiki.page "Disabling-Restyled"
        `shouldBe` "https://github.com/restyled-io/restyled.io/wiki/Disabling-Restyled"

    it "auto-hyphenates" $ do
      Wiki.page "Disabling Restyled"
        `shouldBe` "https://github.com/restyled-io/restyled.io/wiki/Disabling-Restyled"
