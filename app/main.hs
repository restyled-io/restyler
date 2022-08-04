module Main
    ( main
    ) where

import Restyler.Prelude

import Data.Time (getCurrentTime)
import GitHub.Data (toPathPart)
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
    logger <- newLogger oLogSettings
    let tags = [("repo", toPathPart oOwner <> "/" <> toPathPart oRepo)]

    withStatsClient oStatsdHost oStatsdPort tags $ \statsClient -> do
        result <- tryAppError $ do
            withSystemTempDirectory "restyler-" $ \path -> do
                app <- bootstrapApp options logger path statsClient
                runAppT app $ handleAny errorPullRequest restylerMain

        flip runReaderT statsClient $ do
            Statsd.increment "restyler.finished" []
            Statsd.histogramSince "restyler.duration" [] start
            either
                (dieAppError logger) -- includes .error increment
                (\() -> Statsd.increment "restyler.success" [])
                result
