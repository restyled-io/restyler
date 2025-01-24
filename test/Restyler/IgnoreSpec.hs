-- |
--
-- Module      : Restyler.IgnoreSpec
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.IgnoreSpec
  ( spec
  ) where

import Restyler.Prelude

import Restyler.Config
import Restyler.Config.Glob
import Restyler.Ignore
import Test.Hspec

spec :: Spec
spec = do
  describe "getIgnoredReason" $ do
    it "works in empty state" $ do
      let ignores =
            Ignores
              { byAuthor = []
              , byBranch = []
              , byLabels = []
              }

      getIgnoredReason ignores "author" "branch" ["label-a", "label-b"]
        `shouldBe` Nothing

    it "matches authors, then branches, then labels" $ do
      let ignores =
            Ignores
              { byAuthor = [Glob "*[bot]"]
              , byBranch = [Glob "renovate/*"]
              , byLabels = [Glob "wip", Glob "debug"]
              }

      getIgnoredReason ignores "foo[bot]" "branch" []
        `shouldBe` Just (IgnoredByAuthor "foo[bot]")
      getIgnoredReason ignores "foo[bot]" "renovate/foo" []
        `shouldBe` Just (IgnoredByAuthor "foo[bot]")
      getIgnoredReason ignores "foo[bot]" "renovate/foo" ["wip"]
        `shouldBe` Just (IgnoredByAuthor "foo[bot]")
      getIgnoredReason ignores "author" "renovate/foo" []
        `shouldBe` Just (IgnoredByBranch "renovate/foo")
      getIgnoredReason ignores "author" "renovate/foo" ["wip"]
        `shouldBe` Just (IgnoredByBranch "renovate/foo")
      getIgnoredReason ignores "author" "branch" ["wip", "two"]
        `shouldBe` Just (IgnoredByLabels "wip")
      getIgnoredReason ignores "author" "branch" ["one", "debug", "wip"]
        `shouldBe` Just (IgnoredByLabels "debug")
