module Main
  ( main
  ) where

import Restyler.Prelude

import GitHub.Data (toPathPart)
import Restyler.App
import Restyler.Exit
import Restyler.Main
import Restyler.Options
import Restyler.Statsd (withStatsClient)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  options@Options {..} <- parseOptions
  logger <- newLogger oLogSettings
  let tags = [("repo", toPathPart oOwner <> "/" <> toPathPart oRepo)]

  ec <- withStatsClient oStatsdHost oStatsdPort tags $ \statsClient -> do
    withExitHandler logger statsClient options $ do
      withSystemTempDirectory "restyler-" $ \path -> do
        app <- bootstrapApp options logger path statsClient
        runAppT app restylerMain

  runLoggerLoggingT logger
    $ logInfo
    $ "Restyler done"
    :# ["exitCode" .= exitCodeInt ec]

  exitWith ec
