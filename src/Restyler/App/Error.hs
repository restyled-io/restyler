{-# LANGUAGE LambdaCase #-}

module Restyler.App.Error
    ( AppError(..)
    , mapAppError
    , prettyAppError

    -- * Error handling
    , errorPullRequest
    , dieAppErrorHandlers
    )
where

import Restyler.Prelude

import qualified Data.Yaml as Yaml
import GitHub.Data (Error(..))
import Restyler.App.Class
import Restyler.Config (configPath)
import Restyler.Options
import Restyler.PullRequest.Status
import Restyler.Restyler (Restyler(..))
import System.Exit (die)
import Text.Wrap

data AppError
    = PullRequestFetchError Error
    -- ^ We couldn't fetch the @'PullRequest'@ to restyle
    | PullRequestCloneError IOException
    -- ^ We couldn't clone or checkout the PR's branch
    | ConfigurationError Yaml.ParseException
    -- ^ We couldn't load a @.restyled.yaml@
    | RestylerError Restyler IOException
    -- ^ A Restyler we ran exited non-zero
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
    RestylerError r e -> ("with the " <> rName r <> " restyler", show e)
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
reflow = indent . wrap
  where
    indent = unlines . map ("  " <>) . lines
    wrap = unpack . wrapText wrapSettings 78 . pack
    wrapSettings =
        WrapSettings {preserveIndentation = True, breakLongWords = False}

-- | Error the original @'PullRequest'@ and re-throw the exception
errorPullRequest
    :: ( HasLogFunc env
       , HasOptions env
       , HasConfig env
       , HasPullRequest env
       , HasGitHub env
       )
    => SomeException
    -> RIO env ()
errorPullRequest = exceptExit $ \ex -> do
    mJobUrl <- oJobUrl <$> view optionsL
    logInfo $ "Erroring original PR (job-url: " <> displayShow mJobUrl <> ")"
    traverse_ (sendPullRequestStatus_ . ErrorStatus) mJobUrl
    throwIO ex

-- | Error handlers for overall execution
--
-- Usage:
--
-- > {- main routine -} `catches` dieAppErrorHandlers
--
-- Ensures __all__ exceptions (besides @'ExitCode'@s) go through:
--
-- @
-- 'die' . 'prettyAppError'
-- @
--
dieAppErrorHandlers :: [Handler IO ()]
dieAppErrorHandlers =
    [Handler dieAppError, Handler $ exceptExit $ dieAppError . OtherError]

dieAppError :: AppError -> IO a
dieAppError = die . prettyAppError

exceptExit :: Applicative f => (SomeException -> f ()) -> SomeException -> f ()
exceptExit f ex = maybe (f ex) ignore $ fromException ex
  where
    ignore :: Applicative f => ExitCode -> f ()
    ignore _ = pure ()
