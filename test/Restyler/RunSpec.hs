{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Restyler.RunSpec (spec) where

import SpecHelper

import Restyler.Config
import Restyler.Run
import System.Process (callProcess)

spec :: Spec
spec = around (withSystemTempDirectory "") $ do
    describe "callRestylers" $ do
        it "doesn't run on removed files" $ \dir -> do
            setupGitRepo dir
            setupGitTrackedFile "Foo.hs" "" Nothing
            callProcess "git" ["checkout", "--quiet", "-b", "develop"]
            callProcess "git" ["rm", "Foo.hs"]
            callProcess "git" ["commit", "--quiet", "--message", "Remove file"]
            callRestylers defaultConfig ["Foo.hs"]
            ["Foo.hs"] `shouldRestyleAs` []

        context "Default configuration" $ do
            it "restyles Haskell" $ restylerTestCase "Foo.hs"
                [st|
                    {-# LANGUAGE OverloadedStrings, RecordWildcards
                    #-}
                |]
                [ "-{-# LANGUAGE OverloadedStrings, RecordWildcards"
                , "-#-}"
                , "+{-# LANGUAGE OverloadedStrings #-}"
                , "+{-# LANGUAGE RecordWildcards   #-}"
                ]

            it "restyles JavaScript" $ restylerTestCase "foo.js"
                [st|
                    matrix(
                      1, 0, 0,
                      0, 1, 0,
                      0, 0, 1
                    )
                |]
                [ "-matrix("
                , "-  1, 0, 0,"
                , "-  0, 1, 0,"
                , "-  0, 0, 1"
                , "-)"
                , "+matrix(1, 0, 0, 0, 1, 0, 0, 0, 1);"
                ]

        context "non-default configuration" $ do
            describe "hindent" $ do
                it "works" $ \dir -> do
                    setupGitRepo dir
                    setupConfig ["hindent"]
                    setupGitTrackedFile
                        "Foo.hs"
                        "example = case x of Just p -> foo bar\n"
                        $ Just "develop"

                    ["Foo.hs"] `shouldRestyleAs`
                        [ "-example = case x of Just p -> foo bar"
                        , "+example ="
                        , "+  case x of"
                        , "+    Just p -> foo bar"
                        ]

            describe "brittany" $ do
                it "works" $ \dir -> do
                    setupGitRepo dir
                    setupConfig ["brittany"]
                    setupGitTrackedFile
                        "Foo.hs"
                        (dedent [st|
                            func (MyLongFoo abc def) = 1
                            func (Bar a d) = 2
                            func _ = 3
                        |])
                        $ Just "develop"

                    ["Foo.hs"] `shouldRestyleAs`
                        [ " func (MyLongFoo abc def) = 1"
                        , "-func (Bar a d) = 2"
                        , "-func _ = 3"
                        , "+func (Bar       a   d  ) = 2"
                        , "+func _                   = 3"
                        ]

restylerTestCase :: FilePath -> Text -> [String] -> FilePath -> Expectation
restylerTestCase name content changes dir = do
    setupGitRepo dir
    setupGitTrackedFile name (dedent content) $ Just "develop"
    [name] `shouldRestyleAs` changes

shouldRestyleAs :: [FilePath] -> [String] -> Expectation
paths `shouldRestyleAs` changes = either
    (expectationFailure . ("loadConfig: " <>))
    (\config -> do
        callRestylers config paths
        output <- lines <$> readProcess "git" ["diff"] ""
        output `shouldContain` changes
    ) =<< loadConfig
