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
import Text.Wrap

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
prettyAppError = uncurry trouble . \case
    PullRequestFetchError e ->
        ("fetching your Pull Request from GitHub", showGitHubError e)
    PullRequestCloneError e -> ("cloning your Pull Request branch", show e)
    ConfigurationError ex -> ("with your " <> configPath, showConfigError ex)
    DockerError e -> ("with a restyler container", show e)
    GitHubError e -> ("communicating with GitHub", showGitHubError e)
    SystemError e -> ("running a system command", show e)
    HttpError e -> ("performing an HTTP request", show e)
    OtherError e -> ("with something unexpected", show e)

-- |
--
-- @
-- We had trouble {doing a thing}:
--
--   {the full error message...}
--
-- @
--
trouble :: String -> String -> String
trouble acting shown = "We had trouble " <> acting <> ":\n\n" <> reflow shown

showGitHubError :: Error -> String
showGitHubError = \case
    HTTPError e -> "HTTP exception: " <> show e
    ParseError e -> "Unable to parse response: " <> unpack e
    JsonError e -> "Malformed response: " <> unpack e
    UserError e -> "User error: " <> unpack e

showConfigError :: Yaml.ParseException -> String
showConfigError ex = unlines
    [ Yaml.prettyPrintParseException ex
    , "Please see https://github.com/restyled-io/restyled.io/wiki/Common-Errors:-.restyled.yaml"
    ]

reflow :: String -> String
reflow =
    unlines . map ("  " <>) . lines . unpack . wrapText wrapSettings 78 . pack
  where
    wrapSettings =
        WrapSettings {preserveIndentation = True, breakLongWords = True}
