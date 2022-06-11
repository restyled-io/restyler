module Restyler.PullRequestSpecSpec
    ( spec
    ) where

import SpecHelper

import GitHub.Data (IssueNumber(..))
import Restyler.PullRequestSpec

newtype Named = Named Text
    deriving newtype (Eq, Show)

instance Arbitrary Named where
    arbitrary = arbitrary `suchThatMap` mkNamed
      where
        mkNamed :: String -> Maybe Named
        mkNamed s
            | null s = Nothing
            | any isSpace s = Nothing
            | any (`elem` ['/', '#']) s = Nothing
            | otherwise = Just $ Named $ pack s

spec :: Spec
spec = describe "parseSpec" $ do
    it "parses correctly" $ example $ do
        parseSpec "foo/bar#1" `shouldBe` Right (pullRequestSpec "foo" "bar" 1)
        parseSpec "baz/bat#2" `shouldBe` Right (pullRequestSpec "baz" "bat" 2)

    it "errors on invalid input" $ example $ do
        parseSpec "foo/bar" `shouldSatisfy` isLeft
        parseSpec "bar#2" `shouldSatisfy` isLeft
        parseSpec "foo/bar#baz" `shouldSatisfy` isLeft

    it "round-trips" $ property $ \(Named owner, Named name, Positive num) ->
        let prSpec = pullRequestSpec owner name num
        in parseSpec (unpack $ pullRequestSpecToText prSpec) == Right prSpec

pullRequestSpec :: Text -> Text -> Int -> PullRequestSpec
pullRequestSpec owner name num = PullRequestSpec
    { prsOwner = mkName Proxy owner
    , prsRepo = mkName Proxy name
    , prsPullRequest = IssueNumber num
    }
