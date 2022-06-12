module Restyler.App.ErrorSpec
    ( spec
    ) where

import SpecHelper

import Restyler.App.Error

data BigError = BigError
    { beMessage :: Text
    , beDocumentation :: Text
    , beContext :: [Text]
    }
    deriving stock Show
    deriving anyclass Exception

spec :: Spec
spec = do
    describe "prettyError" $ do
        it "wraps really big exceptions (no particularly well)" $ example $ do
            let
                pretty = prettyAppError $ OtherError $ toException BigError
                    { beMessage = "Something has gone terrible, terribly wrong"
                    , beDocumentation =
                        "http://example.com/some/ungodly/long/url?with=all&the=stuff#here-too"
                    , beContext = ["Oof", "Something", "Is", "Really bad"]
                    }

            pretty `shouldBe` mconcat
                [ "We had trouble with something unexpected:"
                , "\n"
                , "\n  BigError {beMessage = \"Something has gone terrible, terribly wrong\","
                , "\n  beDocumentation ="
                , "\n  \"http://example.com/some/ungodly/long/url?with=all&the=stuff#here-too\","
                , "\n  beContext = [\"Oof\",\"Something\",\"Is\",\"Really bad\"]}"
                , "\n"
                , "\n"
                ]
