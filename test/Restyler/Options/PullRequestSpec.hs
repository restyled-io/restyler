module Restyler.Options.PullRequestSpec
  ( spec
  ) where

import Restyler.Prelude

import Restyler.Options.PullRequest
import Restyler.Options.Repository
import Test.Hspec

spec :: Spec
spec = do
  describe "readPullRequest" $ do
    it "reads valid arguments" $ do
      readPullRequest "owner/repo#1"
        `shouldBe` Right
          PullRequestOption
            { repo =
                RepositoryOption
                  { owner = "owner"
                  , repo = "repo"
                  }
            , number = 1
            }

    it "fails on empty owner" $ do
      readPullRequest "/repo#1" `shouldSatisfy` isLeft

    it "fails on empty repo" $ do
      readPullRequest "owner/#1" `shouldSatisfy` isLeft

    it "fails on empty number" $ do
      readPullRequest "repo/owner#" `shouldSatisfy` isLeft

    it "fails on none-number" $ do
      readPullRequest "repo/owner#foo" `shouldSatisfy` isLeft
