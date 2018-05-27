{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.App
    ( App(..)
    , AppM
    , runApp
    , liftIOApp

    -- * Errors
    , AppError(..)
    , mapAppError
    )
where

import Restyler.Prelude.NoApp

import GitHub.Data (Error, PullRequest)
import Restyler.Config (Config)

data App = App
    { appLogLevel :: LogLevel
    , appAccessToken :: Text
    , appPullRequest :: PullRequest
    , appConfig :: Config
    }

type AppM = ReaderT App (LoggingT (ExceptT AppError IO))

data AppError
    = PullRequestFetchError Error
    | PullRequestCloneError IOException
    | ConfigurationError String
    | GitHubError Error
    | OtherError IOException
    deriving Show

runApp :: App -> AppM a -> ExceptT AppError IO a
runApp app@App {..} action =
    runStdoutLoggingT
        $ filterLogger (\_ level -> level >= appLogLevel)
        $ runReaderT action app

-- | @'liftIO'@, but catch @'IOException'@s into @'AppError'@s
--
-- Strive to always use this.
--
liftIOApp :: IO a -> AppM a
liftIOApp = either (throwError . OtherError) pure <=< tryIO . liftIO

mapAppError :: (AppError -> AppError) -> AppM a -> AppM a
mapAppError f = (`catchError` throwError . f)
