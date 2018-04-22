{-# LANGUAGE OverloadedStrings #-}

module Restyler.RepoSpecSpec
    ( spec
    ) where

import SpecHelper

import Restyler.RepoSpec

spec :: Spec
spec = describe "parseRepoSpec" $ do
    it "parses correctly" $ do
        parseRepoSpec "foo/bar#1" `shouldBe` Right (RepoSpec "foo" "bar" 1)
        parseRepoSpec "baz/bat#2" `shouldBe` Right (RepoSpec "baz" "bat" 2)

    it "errors" $ do
        parseRepoSpec "foo/bar" `shouldSatisfy` isLeft
        parseRepoSpec "bar#2" `shouldSatisfy` isLeft
        parseRepoSpec "foo/bar#baz" `shouldSatisfy` isLeft
