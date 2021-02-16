module Main
    ( main
    )
where

import Restyler.Prelude

import Restyler.App
import qualified Restyler.App.Setup as Setup
import qualified Restyler.App.Startup as Startup
import Restyler.AppT
import Restyler.CLI
import Restyler.Main
import Restyler.Options
import Restyler.PullRequest.Status

main :: IO ()
main = restylerCLI parseOptions $ \options path -> do
    let startupApp = Startup.loadApp options path

    runAppT startupApp $ appErrorBoundary $ do
        setupApp <- Setup.loadApp startupApp
        app <- replaceAppT setupApp $ loadApp setupApp
        replaceAppT app
            $ appErrorBoundary restylerMain
            `onError` errorPullRequest
