module Restyler.App.ErrorSpec
    ( spec
    )
where

import SpecHelper

import Restyler.App.Error

data BigError = BigError
    { beMessage :: String
    , beDocumentation :: String
    , beContext :: [String]
    }
    deriving Show

instance Exception BigError

spec :: Spec
spec = do
    describe "prettyError" $ do
        it "wraps really big exceptions (no particularly well)" $ do
            let
                pretty = prettyAppError $ OtherError $ toException BigError
                    { beMessage = "Something has gone terrible, terribly wrong"
                    , beDocumentation = "http://example.com/some/ungodly/long/url?with=all&the=stuff#here-too"
                    , beContext = ["Oof", "Something", "Is", "Really bad"]
                    }

            pretty `shouldBe` concat
                [ "We had trouble with something unexpected:"
                , "\n"
                , "\n  BigError {beMessage = \"Something has gone terrible, terribly wrong\","
                , "\n  beDocumentation ="
                , "\n  \"http://example.com/some/ungodly/long/url?with=all&the=stuff#here-too\","
                , "\n  beContext = [\"Oof\",\"Something\",\"Is\",\"Really bad\"]}"
                , "\n"
                , "\n"
                ]

    describe "scrubGitHubToken" $ do
        it "scrubs clone errors of ephemeral tokens" $ do
            let errorMessage = concat
                    [ "\nBranch 'if-non-unique-operator-channel-id' set up to track remote branch 'if-non-unique-operator-...skipping..."
                    , "\nerror: failed to push some refs to 'https://x-access-token:v1.aaaaaaaaaabbbbbbbbbbbbbbbcccccccccdddddd@github.com/Foo/bar.git'"
                    , "\n ! [rejected]        refactor-modules-restyled -> refactor-modules-restyled (stale info)"
                    , "\nTo https://github.com/Foo/bar.git"
                    ]

                scrubbed = scrubGitHubToken errorMessage

            scrubbed `shouldBe` concat
                [ "\nBranch 'if-non-unique-operator-channel-id' set up to track remote branch 'if-non-unique-operator-...skipping..."
                , "\nerror: failed to push some refs to 'https://<SCRUBBED>@github.com/Foo/bar.git'"
                , "\n ! [rejected]        refactor-modules-restyled -> refactor-modules-restyled (stale info)"
                , "\nTo https://github.com/Foo/bar.git"
                ]
