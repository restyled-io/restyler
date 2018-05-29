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

-- | Application environment
data App = App
    { appLogLevel :: LogLevel
    , appAccessToken :: Text
    , appPullRequest :: PullRequest
    -- ^ The @'PullRequest'@ we are restyling
    , appConfig :: Config
    -- ^ Configuration loaded from @.restyled.yaml@
    }

-- | A type alias for our Reader-Logging-Except-IO stack
type AppM = ReaderT App (LoggingT (ExceptT AppError IO))

-- | All possible application error conditions
data AppError
    = PullRequestFetchError Error
    -- ^ We couldn't fetch the @'PullRequest'@ to restyle
    | PullRequestCloneError IOException
    -- ^ We couldn't clone or checkout the PR's branch
    | ConfigurationError String
    -- ^ We couldn't load a @.restyled.yaml@
    | GitHubError Error
    -- ^ We encountered a GitHub API error during restyling
    | OtherError IOException
    -- ^ A minor escape hatch for @'IOException'@s
    deriving Show

-- | Run an @'AppM' a@, producing an @'AppError'@ or an @a@
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

-- | Run a computation, and modify any thrown @'AppError'@s
mapAppError :: (AppError -> AppError) -> AppM a -> AppM a
mapAppError f = (`catchError` throwError . f)
