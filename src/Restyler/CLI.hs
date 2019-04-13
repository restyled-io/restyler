module Restyler.CLI
    ( restylerCLI
    )
where

import Restyler.Prelude

import Restyler.App
import Restyler.App.Class
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
        runRIO app $ restylerMain `catchAny` errorPullRequest (oJobUrl options)

errorPullRequest
    :: (HasLogFunc env, HasConfig env, HasPullRequest env, HasGitHub env)
    => Maybe URL
    -> SomeException
    -> RIO env ()
errorPullRequest mJobUrl = exceptExit $ \ex -> do
    traverse_ (sendPullRequestStatus_ . ErrorStatus) mJobUrl
    throwIO ex

dieHandlers :: [Handler IO ()]
dieHandlers =
    [Handler dieAppError, Handler $ exceptExit $ dieAppError . OtherError]

dieAppError :: AppError -> IO ()
dieAppError = die . prettyAppError

-- | Run the given handler, unless this was an @'ExitCode'@ exception
exceptExit :: Applicative f => (SomeException -> f ()) -> SomeException -> f ()
exceptExit f ex = maybe (f ex) ignore $ fromException ex
  where
    ignore :: Applicative f => ExitCode -> f ()
    ignore _ = pure ()

-- | Flip @'catches'@, for convenience
handles :: [Handler IO a] -> IO a -> IO a
handles = flip catches
