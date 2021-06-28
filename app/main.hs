module Main
    ( main
    )
where

import Restyler.Prelude

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
    handles dieAppErrorHandlers
        $ withSystemTempDirectory "restyler-"
        $ \path -> withStatsClient oStatsdHost oStatsdPort $ \statsClient -> do
              app <- bootstrapApp options path statsClient
              runRIO app $ handleAny errorPullRequest restylerMain
