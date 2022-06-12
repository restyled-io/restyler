module Restyler.App.Error
    ( AppError(..)
    , mapAppError
    , prettyAppError

    -- * Error handling
    , errorPullRequest
    , tryAppError
    , dieAppError

    -- * Lower-level helpers
    , warnIgnore
    ) where

import Restyler.Prelude

import qualified Data.Text.IO as T
import qualified Data.Yaml as Yaml
import GitHub.Data (Error(..))
import GitHub.Request.Display
import Restyler.App.Class
import Restyler.Config
import Restyler.Options
import Restyler.PullRequest
import Restyler.PullRequest.Status
import Restyler.Restyler (Restyler(..))
import Restyler.Statsd (HasStatsClient)
import qualified Restyler.Statsd as Statsd
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

prettyAppError :: AppError -> Text
prettyAppError =
    format <$> toErrorTitle <*> toErrorBody <*> toErrorDocumentation
  where
    format :: Text -> Text -> Text -> Text
    format title body docs = title <> ":\n\n" <> body <> docs

toErrorTitle :: AppError -> Text
toErrorTitle = trouble . \case
    PullRequestFetchError _ -> "fetching your Pull Request from GitHub"
    PullRequestCloneError _ -> "cloning your Pull Request branch"
    ConfigurationError _ -> "with your configuration"
    RestylerExitFailure r _ _ -> "with the " <> pack (rName r) <> " restyler"
    RestyleError _ -> "restyling"
    GitHubError _ _ -> "communicating with GitHub"
    SystemError _ -> "running a system command"
    HttpError _ -> "performing an HTTP request"
    OtherError _ -> "with something unexpected"
  where
    trouble :: Text -> Text
    trouble = ("We had trouble " <>)

toErrorBody :: AppError -> Text
toErrorBody = reflow . \case
    PullRequestFetchError e -> showGitHubError e
    PullRequestCloneError e -> show e
    ConfigurationError (ConfigErrorInvalidYaml yaml e) -> unlines
        [ "Yaml parse exception:"
        , pack $ Yaml.prettyPrintParseException e
        , ""
        , "Original input:"
        , decodeUtf8 yaml
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
    RestyleError msg -> msg
    GitHubError req e -> "Request: " <> show req <> "\n" <> showGitHubError e
    SystemError e -> show e
    HttpError e -> show e
    OtherError e -> show e

toErrorDocumentation :: AppError -> Text
toErrorDocumentation = formatDocs . \case
    ConfigurationError ConfigErrorInvalidRestylersYaml{} ->
        ["https://github.com/restyled-io/restyled.io/wiki/Restyler-Versions"]
    ConfigurationError _ ->
        [ "https://github.com/restyled-io/restyled.io/wiki/Common-Errors:-.restyled.yaml"
        ]
    RestylerExitFailure r _ _ -> map pack $ rDocumentation r
    RestyleError _ ->
        [ "https://github.com/restyled-io/restyled.io/wiki/Common-Errors:-Restyle-Error"
        ]
    _ -> []
  where
    formatDocs [] = "\n"
    formatDocs [url] = "\nPlease see " <> url <> "\n"
    formatDocs urls = unlines $ "\nPlease see" : map ("  - " <>) urls

showGitHubError :: Error -> Text
showGitHubError = \case
    HTTPError e -> "HTTP exception: " <> show e
    ParseError e -> "Unable to parse response: " <> e
    JsonError e -> "Malformed response: " <> e
    UserError e -> "User error: " <> e

reflow :: Text -> Text
reflow = indent . wrap
  where
    indent = unlines . map ("  " <>) . lines
    wrap = wrapText wrapSettings 80
    wrapSettings = defaultWrapSettings
        { preserveIndentation = True
        , breakLongWords = False
        }

-- | Error the original @'PullRequest'@ and re-throw the exception
errorPullRequest
    :: ( MonadUnliftIO m
       , MonadLogger m
       , MonadGitHub m
       , MonadReader env m
       , HasOptions env
       , HasConfig env
       , HasPullRequest env
       )
    => SomeException
    -> m ()
errorPullRequest = exceptExit $ \ex -> do
    mJobUrl <- oJobUrl <$> view optionsL
    traverse_ errorPullRequestUrl mJobUrl
    throwIO ex

-- | Actually error the @'PullRequest'@, given the job-url to link to
errorPullRequestUrl
    :: ( MonadUnliftIO m
       , MonadLogger m
       , MonadGitHub m
       , MonadReader env m
       , HasConfig env
       , HasPullRequest env
       )
    => URL
    -> m ()
errorPullRequestUrl url =
    handleAny warnIgnore $ sendPullRequestStatus $ ErrorStatus url

-- | Ignore an exception, warning about it.
warnIgnore :: (MonadLogger m, Exception e) => e -> m ()
warnIgnore ex =
    logWarn
        $ "Ignoring caught exception"
        :# ["exception" .= displayException ex]

tryAppError :: MonadUnliftIO m => m a -> m (Either AppError ())
tryAppError f = handles appErrorHandlers $ do
    void f
    pure $ Right ()

appErrorHandlers :: Applicative f => [Handler f (Either AppError ())]
appErrorHandlers =
    [ Handler $ \(_ex :: ExitCode) -> pure $ Right ()
    , Handler $ \(err :: AppError) -> pure $ Left err
    , Handler $ \err -> pure $ Left $ OtherError err
    ]

dieAppError
    :: (MonadIO m, MonadReader env m, HasStatsClient env) => AppError -> m a
dieAppError e = do
    liftIO $ T.hPutStrLn stderr $ prettyAppError e
    let tags = [("severity", severityTag), ("error", errorTag)]
    Statsd.increment "restyler.error" tags
    exitWith $ ExitFailure exitCode
  where
    (severityTag, errorTag, exitCode) = case e of
        ConfigurationError ConfigErrorInvalidYaml{} ->
            ("warning", "invalid-config", 10)
        ConfigurationError ConfigErrorInvalidRestylers{} ->
            ("warning", "invalid-config-restylers", 11)
        ConfigurationError ConfigErrorInvalidRestylersYaml{} ->
            ("error", "invalid-restylers-yaml", 12)
        RestylerExitFailure{} -> ("warning", "restyler", 20)
        RestyleError{} -> ("warning", "restyle-error", 25)
        GitHubError{} -> ("warning", "github", 30)
        PullRequestFetchError{} -> ("warning", "fetch", 31)
        PullRequestCloneError{} -> ("warning", "clone", 32)
        HttpError{} -> ("error", "http", 40)
        SystemError{} -> ("error", "system", 50)
        OtherError{} -> ("critical", "unknown", 99)

exceptExit :: Applicative f => (SomeException -> f ()) -> SomeException -> f ()
exceptExit f ex = maybe (f ex) ignore $ fromException ex
  where
    ignore :: Applicative f => ExitCode -> f ()
    ignore _ = pure ()
