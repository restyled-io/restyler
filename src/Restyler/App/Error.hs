{-# LANGUAGE LambdaCase #-}

module Restyler.App.Error
    ( AppError(..)
    , mapAppError
    , prettyAppError
    )
where

import Restyler.Prelude

import qualified Data.Yaml as Yaml
import Restyler.Config (configPath)

data AppError
    = PullRequestFetchError Error
    -- ^ We couldn't fetch the @'PullRequest'@ to restyle
    | PullRequestCloneError IOException
    -- ^ We couldn't clone or checkout the PR's branch
    | ConfigurationError Yaml.ParseException
    -- ^ We couldn't load a @.restyled.yaml@
    | DockerError IOException
    -- ^ Error running a @docker@ operation
    | GitHubError Error
    -- ^ We encountered a GitHub API error during restyling
    | SystemError IOException
    -- ^ Trouble reading a file or etc
    | HttpError IOException
    -- ^ Trouble performing some HTTP request
    | OtherError SomeException
    -- ^ Escape hatch for anything else
    deriving Show

instance Exception AppError

-- | Run a computation, and modify any thrown exceptions to @'AppError'@s
mapAppError :: (MonadUnliftIO m, Exception e) => (e -> AppError) -> m a -> m a
mapAppError f = handle $ throwIO . f

prettyAppError :: AppError -> String
prettyAppError = format . \case
    PullRequestFetchError e ->
        [ "We had trouble fetching your Pull Request from GitHub:"
        , showGitHubError e
        ]
    PullRequestCloneError e ->
        ["We had trouble cloning your Pull Request branch:" <> show e]
    ConfigurationError ex ->
        [ "We had trouble with your "
            <> configPath
            <> ": "
            <> Yaml.prettyPrintParseException ex
        , "Please see https://github.com/restyled-io/restyled.io/wiki/Common-Errors:-.restyled.yaml"
        ]
    DockerError e -> ["The restyler container exited non-zero:", show e]
    GitHubError e ->
        ["We had trouble communicating with GitHub:", showGitHubError e]
    SystemError e -> ["We had trouble running a system command:", show e]
    HttpError e -> ["We had trouble performing an HTTP request:", show e]
    OtherError e -> ["We encountered an unexpected exception:", show e]
  where
    format msg = "[Error] " <> dropWhileEnd isSpace (unlines msg)
    showGitHubError (HTTPError e) = "HTTP exception: " <> show e
    showGitHubError (ParseError e) = "Unable to parse response: " <> unpack e
    showGitHubError (JsonError e) = "Malformed response: " <> unpack e
    showGitHubError (UserError e) = "User error: " <> unpack e
