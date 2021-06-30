module Main
    ( main
    ) where

import Restyler.Prelude

import GitHub.Data (toPathPart)
import Restyler.App
import Restyler.App.Error
import Restyler.Main
import Restyler.Options
import Restyler.Statsd (withStatsClient)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    options@Options {..} <- parseOptions
    let tags = [("repo", toPathPart oOwner <> "/" <> toPathPart oRepo)]

    withStatsClient oStatsdHost oStatsdPort tags $ \statsClient -> do
        result <- tryAppError $ do
            withSystemTempDirectory "restyler-" $ \path -> do
                app <- bootstrapApp options path statsClient
                runRIO app $ handleAny errorPullRequest restylerMain
        runRIO statsClient $ either dieAppError pure result
