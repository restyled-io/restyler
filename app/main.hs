module Main
    ( main
    ) where

import Restyler.Prelude

import GitHub.Data (toPathPart)
import RIO.Time (getCurrentTime)
import Restyler.App
import Restyler.App.Error
import Restyler.Main
import Restyler.Options
import Restyler.Statsd (withStatsClient)
import qualified Restyler.Statsd as Statsd

main :: IO ()
main = do
    start <- getCurrentTime
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    options@Options {..} <- parseOptions
    let tags = [("repo", toPathPart oOwner <> "/" <> toPathPart oRepo)]

    withStatsClient oStatsdHost oStatsdPort tags $ \statsClient -> do
        result <- tryAppError $ do
            withSystemTempDirectory "restyler-" $ \path -> do
                app <- bootstrapApp options path statsClient
                runRIO app $ handleAny errorPullRequest restylerMain

        runRIO statsClient $ do
            Statsd.increment "restyler.finished" []
            Statsd.histogramSince "restyler.duration" [] start
            either
                dieAppError -- includes .error increment
                (\() -> Statsd.increment "restyler.success" [])
                result
