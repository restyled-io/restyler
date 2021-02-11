module Restyler.App.Error
    ( AppError(..)
    , prettyAppError
    , ErrorDetails(..)
    , appErrorDetails
    )
where

import Restyler.Prelude

import qualified Data.Yaml as Yaml
import GitHub.Data (Error(..))
import GitHub.Request.Display
import Network.HTTP.Client (HttpException)
import Restyler.Restyler (Restyler(..))
import Text.Wrap

data AppError
    = PullRequestFetchError Error
    -- ^ We couldn't fetch the @'PullRequest'@ to restyle
    | PullRequestCloneError IOException
    -- ^ We couldn't clone or checkout the PR's branch
    | ConfigErrorInvalidYaml ByteString Yaml.ParseException
    -- ^ We couldn't load a @.restyled.yaml@ at all
    | ConfigErrorInvalidRestylers [String]
    -- ^ We couldn't load a @.restyled.yaml@'s Restylers
    | ConfigErrorInvalidRestylersYaml Yaml.ParseException
    -- ^ We couldn't load /our/ @restylers.yaml@
    | RestylerExitFailure Restyler Int [FilePath]
    -- ^ A Restyler we ran exited non-zero on the given paths
    | RestyleError Text
    -- ^ Unable to Restyle for a known reason (given as user-facing message)
    | GitHubError DisplayGitHubRequest Error
    -- ^ We encountered a GitHub API error during restyling
    | SystemError IOException
    -- ^ Trouble reading a file or etc
    | HttpError HttpException
    -- ^ Trouble performing some HTTP request
    | OtherError SomeException
    -- ^ Escape hatch for anything else
    deriving stock Show

instance Exception AppError where
    displayException = prettyAppError

prettyAppError :: AppError -> String
prettyAppError = formatErrorDetails . appErrorDetails

data ErrorDetails = ErrorDetails
    { edActionAttempted :: String
    , edDetails :: String
    , edDocumentationUrls :: [String]
    , edExitCode :: Int
    }

formatErrorDetails :: ErrorDetails -> String
formatErrorDetails ErrorDetails {..} =
    "We had trouble "
        <> edActionAttempted
        <> ":\n\n"
        <> reflow edDetails
        <> formatDocs edDocumentationUrls
  where
    formatDocs [] = "\n"
    formatDocs [url] = "\nPlease see " <> url <> "\n"
    formatDocs urls = unlines $ "\nPlease see" : map ("  - " <>) urls

appErrorDetails :: AppError -> ErrorDetails
appErrorDetails = \case
    PullRequestFetchError e -> ErrorDetails
        { edActionAttempted = "fetching your Pull Request from GitHub"
        , edDetails = githubErrorDetails e
        , edDocumentationUrls = []
        , edExitCode = 31
        }
    PullRequestCloneError e -> ErrorDetails
        { edActionAttempted = "cloning your Pull Request branch"
        , edDetails = show e
        , edDocumentationUrls = []
        , edExitCode = 32
        }
    ConfigErrorInvalidYaml yaml e -> ErrorDetails
        { edActionAttempted = "with your configuration"
        , edDetails = unlines
            [ "Yaml parse exception:"
            , Yaml.prettyPrintParseException e
            , ""
            , "Original input:"
            , unpack $ decodeUtf8 yaml
            ]
        , edDocumentationUrls =
            [ "https://github.com/restyled-io/restyled.io/wiki/Common-Errors:-.restyled.yaml"
            ]
        , edExitCode = 10
        }
    ConfigErrorInvalidRestylers errs -> ErrorDetails
        { edActionAttempted = "with your configured Restylers"
        , edDetails = unlines errs
        , edDocumentationUrls =
            [ "https://github.com/restyled-io/restyled.io/wiki/Common-Errors:-.restyled.yaml"
            ]
        , edExitCode = 11
        }
    ConfigErrorInvalidRestylersYaml e -> ErrorDetails
        { edActionAttempted = "with our Restylers definitions"
        , edDetails = unlines
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
        , edDocumentationUrls =
            [ "https://github.com/restyled-io/restyled.io/wiki/Restyler-Versions"
            ]
        , edExitCode = 12
        }
    RestylerExitFailure r s paths -> ErrorDetails
        { edActionAttempted = "with the " <> rName r <> " restyler"
        , edDetails =
            "Exited non-zero ("
            <> show s
            <> ") for the following paths, "
            <> show paths
            <> "."
            <> "\nError information may be present in the stderr output above."
        , edDocumentationUrls = rDocumentation r
        , edExitCode = 20
        }
    RestyleError msg -> ErrorDetails
        { edActionAttempted = "restyling"
        , edDetails = unpack msg
        , edDocumentationUrls =
            [ "https://github.com/restyled-io/restyled.io/wiki/Common-Errors:-Restyle-Error"
            ]
        , edExitCode = 25
        }
    GitHubError req e -> ErrorDetails
        { edActionAttempted = "communicating with GitHub"
        , edDetails = "Request: " <> show req <> "\n" <> githubErrorDetails e
        , edDocumentationUrls = []
        , edExitCode = 30
        }
    SystemError e -> ErrorDetails
        { edActionAttempted = "running a system command"
        , edDetails = show e
        , edDocumentationUrls = []
        , edExitCode = 50
        }
    HttpError e -> ErrorDetails
        { edActionAttempted = "performing an HTTP request"
        , edDetails = show e
        , edDocumentationUrls = []
        , edExitCode = 40
        }
    OtherError e -> ErrorDetails
        { edActionAttempted = "with something unexpected"
        , edDetails = show e
        , edDocumentationUrls = []
        , edExitCode = 99
        }

githubErrorDetails :: Error -> String
githubErrorDetails = \case
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
