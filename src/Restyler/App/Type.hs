{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Restyler.App.Type
    ( App(..)

    -- * Application errors
    , AppError(..)
    , mapAppError

    -- * Concrete Application stack
    , AppT(..)
    ) where

import Restyler.Prelude

import Restyler.Model.Config
import Restyler.Options

-- | Application environment
data App = App
    { appLogLevel :: LogLevel
    , appLogColor :: Bool
    , appAccessToken :: Text
    , appPullRequest :: PullRequest
    -- ^ The @'PullRequest'@ we are restyling
    , appRestyledPullRequest :: Maybe SimplePullRequest
    -- ^ Existing restyled @'PullRequest'@ if it exists
    , appConfig :: Config
    -- ^ Configuration loaded from @.restyled.yaml@
    , appOptions :: Options
    -- ^ Original command-line options
    , appWorkingDirectory :: FilePath
    -- ^ Temporary directory we are working in
    }

-- | All possible application error conditions
data AppError
    = PullRequestFetchError Error
    -- ^ We couldn't fetch the @'PullRequest'@ to restyle
    | PullRequestCloneError IOException
    -- ^ We couldn't clone or checkout the PR's branch
    | ConfigurationError String
    -- ^ We couldn't load a @.restyled.yaml@
    | DockerError IOException
    -- ^ Error running a @docker@ operation
    | GitHubError Error
    -- ^ We encountered a GitHub API error during restyling
    | SystemError IOException
    -- ^ Trouble reading a file or etc
    | HttpError IOException
    -- ^ Trouble performing some HTTP request
    | OtherError IOException
    -- ^ A minor escape hatch for @'IOException'@s
    deriving Show

-- | Run a computation, and modify any thrown @'AppError'@s
mapAppError :: MonadError AppError m => (AppError -> AppError) -> m a -> m a
mapAppError f = (`catchError` throwError . f)

newtype AppT m a = AppT
    { runAppT :: ReaderT App (LoggingT (ExceptT AppError m)) a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadError AppError
        , MonadReader App
        , MonadLogger
        )
