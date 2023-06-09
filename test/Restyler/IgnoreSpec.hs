module Restyler.IgnoreSpec
  ( spec
  ) where

import SpecHelper

import Restyler.Config
import Restyler.Config.Glob
import Restyler.Ignore

spec :: Spec
spec = do
  describe "getIgnoredReason" $ do
    it "works in empty state" $ do
      config <-
        loadModifiedConfig $
          \c ->
            c
              { cIgnoreAuthors = []
              , cIgnoreBranches = []
              , cIgnoreLabels = []
              }

      getIgnoredReason' config "author" "branch" ["label-a", "label-b"]
        `shouldBe` Nothing

    it "matches authors, then branches, then labels" $ do
      config <- loadModifiedConfig $ \c ->
        c
          { cIgnoreAuthors = [Glob "*[bot]"]
          , cIgnoreBranches = [Glob "renovate/*"]
          , cIgnoreLabels = [Glob "wip", Glob "debug"]
          }

      getIgnoredReason' config "foo[bot]" "branch" []
        `shouldBe` Just (IgnoredByAuthor "foo[bot]")
      getIgnoredReason' config "foo[bot]" "renovate/foo" []
        `shouldBe` Just (IgnoredByAuthor "foo[bot]")
      getIgnoredReason' config "foo[bot]" "renovate/foo" ["wip"]
        `shouldBe` Just (IgnoredByAuthor "foo[bot]")
      getIgnoredReason' config "author" "renovate/foo" []
        `shouldBe` Just (IgnoredByBranch "renovate/foo")
      getIgnoredReason' config "author" "renovate/foo" ["wip"]
        `shouldBe` Just (IgnoredByBranch "renovate/foo")
      getIgnoredReason' config "author" "branch" ["wip", "two"]
        `shouldBe` Just (IgnoredByLabels "wip")
      getIgnoredReason' config "author" "branch" ["one", "debug", "wip"]
        `shouldBe` Just (IgnoredByLabels "debug")

loadModifiedConfig :: (Config -> Config) -> IO Config
loadModifiedConfig f = liftIO $ f <$> loadDefaultConfig
