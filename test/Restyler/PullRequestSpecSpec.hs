module Restyler.PullRequestSpecSpec
    ( spec
    )
where

import SpecHelper

import GitHub.Data (IssueNumber(..))
import Restyler.PullRequestSpec

newtype Named = Named Text
    deriving newtype (Eq, Show)

instance Arbitrary Named where
    arbitrary = Named . pack <$> arbitrary `suchThat` all goodChar
      where
        goodChar c
            | isSpace c = False
            | otherwise = c `notElem` ['/', '#']

spec :: Spec
spec = describe "parseSpec" $ do
    it "parses correctly" $ do
        parseSpec "foo/bar#1" `shouldBe` Right (pullRequestSpec "foo" "bar" 1)
        parseSpec "baz/bat#2" `shouldBe` Right (pullRequestSpec "baz" "bat" 2)

    it "errors on invalid input" $ do
        parseSpec "foo/bar" `shouldSatisfy` isLeft
        parseSpec "bar#2" `shouldSatisfy` isLeft
        parseSpec "foo/bar#baz" `shouldSatisfy` isLeft

    it "round-trips" $ property $ \(Named owner, Named name, Positive num) ->
        let prSpec = pullRequestSpec owner name num
        in parseSpec (show prSpec) == Right prSpec

pullRequestSpec :: Text -> Text -> Int -> PullRequestSpec
pullRequestSpec owner name num = PullRequestSpec
    { prsOwner = mkName Proxy owner
    , prsRepo = mkName Proxy name
    , prsPullRequest = IssueNumber num
    }
