module Main (main) where

import Restyler.Prelude

import Restyler.App
import Restyler.App.Error
import Restyler.Main
import Restyler.Options

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    options <- parseOptions
    handles dieAppErrorHandlers
        $ withSystemTempDirectory "restyler-"
        $ \path -> do
              app <- bootstrapApp options path
              runRIO app $ handleAny errorPullRequest restylerMain
