module Restyler.CLI
    ( restylerCLI
    )
where

import Restyler.Prelude

import Restyler.App
import Restyler.App.Error
import Restyler.Main
import Restyler.Options
import Restyler.PullRequest.Status
import System.Exit (die)

restylerCLI :: IO ()
restylerCLI = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    options <- parseOptions

    handles dieHandlers $ withSystemTempDirectory "restyler-" $ \path -> do
        app <- bootstrapApp options path
        runRIO app $ restylerMain `catchIO` \ex -> do
            traverse_ (sendPullRequestStatus_ . ErrorStatus) $ oJobUrl options
            throwIO ex

handles :: [Handler IO a] -> IO a -> IO a
handles = flip catches

dieHandlers :: [Handler IO a]
dieHandlers = [Handler dieAppError, Handler $ dieAppError . OtherError]

dieAppError :: AppError -> IO a
dieAppError = die . prettyAppError
