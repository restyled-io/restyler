module Restyler.App.Error
    ( AppError(..)
    , mapAppError
    , prettyAppError

    -- * Error handling
    , errorPullRequest
    , dieAppErrorHandlers

    -- * Lower-level helpers
    , warnIgnore
    )
where

import Restyler.Prelude

import qualified Data.Yaml as Yaml
import GitHub.Data (Error(..))
import GitHub.Request.Display
import Restyler.App.Class
import Restyler.Config
import Restyler.Options
import Restyler.PullRequest
import Restyler.PullRequest.Status
import Restyler.Restyler (Restyler(..))
import System.IO (hPutStrLn)
import Text.Wrap

data AppError
    = PullRequestFetchError Error
    -- ^ We couldn't fetch the @'PullRequest'@ to restyle
    | PullRequestCloneError IOException
    -- ^ We couldn't clone or checkout the PR's branch
    | ConfigurationError ConfigError
    -- ^ We couldn't load a @.restyled.yaml@
    | RestylerExitFailure Restyler Int [FilePath]
    -- ^ A Restyler we ran exited non-zero on the given paths
    | RestyleError Text
    -- ^ Unable to Restyle for a known reason (given as user-facing message)
    | GitHubError DisplayGitHubRequest Error
    -- ^ We encountered a GitHub API error during restyling
    | SystemError IOException
    -- ^ Trouble reading a file or etc
    | HttpError IOException
    -- ^ Trouble performing some HTTP request
    | OtherError SomeException
    -- ^ Escape hatch for anything else
    deriving stock Show

instance Exception AppError

-- | Run a computation, and modify any thrown exceptions to @'AppError'@s
mapAppError :: (MonadUnliftIO m, Exception e) => (e -> AppError) -> m a -> m a
mapAppError f = handle $ throwIO . f

prettyAppError :: AppError -> String
prettyAppError =
    format <$> toErrorTitle <*> toErrorBody <*> toErrorDocumentation
  where
    format :: String -> String -> String -> String
    format title body docs = title <> ":\n\n" <> body <> docs

toErrorTitle :: AppError -> String
toErrorTitle = trouble . \case
    PullRequestFetchError _ -> "fetching your Pull Request from GitHub"
    PullRequestCloneError _ -> "cloning your Pull Request branch"
    ConfigurationError _ -> "with your configuration"
    RestylerExitFailure r _ _ -> "with the " <> rName r <> " restyler"
    RestyleError _ -> "restyling"
    GitHubError _ _ -> "communicating with GitHub"
    SystemError _ -> "running a system command"
    HttpError _ -> "performing an HTTP request"
    OtherError _ -> "with something unexpected"
  where
    trouble :: String -> String
    trouble = ("We had trouble " <>)

toErrorBody :: AppError -> String
toErrorBody = reflow . \case
    PullRequestFetchError e -> showGitHubError e
    PullRequestCloneError e -> show e
    ConfigurationError (ConfigErrorInvalidYaml yaml e) -> unlines
        [ "Yaml parse exception:"
        , Yaml.prettyPrintParseException e
        , ""
        , "Original input:"
        , unpack $ decodeUtf8 yaml
        ]
    ConfigurationError (ConfigErrorInvalidRestylers errs) -> unlines errs
    ConfigurationError (ConfigErrorInvalidRestylersYaml e) -> unlines
        [ "Error loading restylers.yaml definition:"
        , show e
        , ""
        , "==="
        , ""
        , "This could be caused by an invalid or too-old restylers_version in"
        , "your configuration. Consider removing or updating it."
        , ""
        , "If that's not the case, this is a bug in our system that we are"
        , "hopefully already working to fix."
        ]
    RestylerExitFailure _ s paths ->
        "Exited non-zero ("
            <> show s
            <> ") for the following paths, "
            <> show paths
            <> "."
            <> "\nError information may be present in the stderr output above."
    RestyleError msg -> unpack msg
    GitHubError req e -> "Request: " <> show req <> "\n" <> showGitHubError e
    SystemError e -> show e
    HttpError e -> show e
    OtherError e -> show e

toErrorDocumentation :: AppError -> String
toErrorDocumentation = formatDocs . \case
    ConfigurationError ConfigErrorInvalidRestylersYaml{} ->
        ["https://github.com/restyled-io/restyled.io/wiki/Restyler-Versions"]
    ConfigurationError _ ->
        [ "https://github.com/restyled-io/restyled.io/wiki/Common-Errors:-.restyled.yaml"
        ]
    RestylerExitFailure r _ _ -> rDocumentation r
    RestyleError _ ->
        [ "https://github.com/restyled-io/restyled.io/wiki/Common-Errors:-Restyle-Error"
        ]
    _ -> []
  where
    formatDocs [] = "\n"
    formatDocs [url] = "\nPlease see " <> url <> "\n"
    formatDocs urls = unlines $ "\nPlease see" : map ("  - " <>) urls

showGitHubError :: Error -> String
showGitHubError = \case
    HTTPError e -> "HTTP exception: " <> show e
    ParseError e -> "Unable to parse response: " <> unpack e
    JsonError e -> "Malformed response: " <> unpack e
    UserError e -> "User error: " <> unpack e

reflow :: String -> String
reflow = indent . wrap
  where
    indent = unlines . map ("  " <>) . lines
    wrap = unpack . wrapText wrapSettings 80 . pack
    wrapSettings =
        WrapSettings { preserveIndentation = True, breakLongWords = False }

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
    traverse_ errorPullRequestUrl mJobUrl
    throwIO ex

-- | Actually error the @'PullRequest'@, given the job-url to link to
errorPullRequestUrl
    :: (HasLogFunc env, HasConfig env, HasPullRequest env, HasGitHub env)
    => URL
    -> RIO env ()
errorPullRequestUrl url =
    handleAny warnIgnore $ sendPullRequestStatus $ ErrorStatus url

-- | Ignore an exception, warning about it.
warnIgnore :: (Display a, HasLogFunc env) => a -> RIO env ()
warnIgnore ex = logWarn $ "Caught " <> display ex <> ", ignoring."

-- | Error handlers for overall execution
--
-- Usage:
--
-- > {- main routine -} `catches` dieAppErrorHandlers
--
-- Ensures __all__ exceptions (besides @'ExitCode'@s) go through:
--
-- @
-- 'dieAppError'
-- @
--
dieAppErrorHandlers :: [Handler IO ()]
dieAppErrorHandlers =
    [Handler dieAppError, Handler $ exceptExit $ dieAppError . OtherError]

dieAppError :: AppError -> IO a
dieAppError e = do
    hPutStrLn stderr $ prettyAppError e
    exitWith $ ExitFailure $ case e of
        ConfigurationError ConfigErrorInvalidYaml{} -> 10
        ConfigurationError ConfigErrorInvalidRestylers{} -> 11
        ConfigurationError ConfigErrorInvalidRestylersYaml{} -> 12
        RestylerExitFailure{} -> 20
        RestyleError{} -> 25
        GitHubError{} -> 30
        PullRequestFetchError{} -> 31
        PullRequestCloneError{} -> 32
        HttpError{} -> 40
        SystemError{} -> 50
        OtherError{} -> 99

exceptExit :: Applicative f => (SomeException -> f ()) -> SomeException -> f ()
exceptExit f ex = maybe (f ex) ignore $ fromException ex
  where
    ignore :: Applicative f => ExitCode -> f ()
    ignore _ = pure ()
