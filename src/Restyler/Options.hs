module Restyler.Options
    ( Options(..)
    , HasOptions(..)
    , parseOptions
    ) where

import Restyler.Prelude

import Blammo.Logging.LogSettings
import qualified Blammo.Logging.LogSettings.Env as LoggingEnv
import Blammo.Logging.LogSettings.LogLevels
import qualified Env
import GitHub.Data (IssueNumber, Owner, Repo)
import Options.Applicative
import Restyler.PullRequestSpec

data EnvOptions = EnvOptions
    { eoAccessToken :: Text
    , eoLogLevel :: Maybe LogLevel
    -- ^ Deprecated
    , eoLogSettings :: LogSettings
    , eoUnrestricted :: Bool
    , eoStatsdHost :: Maybe String
    , eoStatsdPort :: Maybe Int
    }

data CLIOptions = CLIOptions
    { coColor :: Maybe LogColor
    , coJobUrl :: Maybe URL
    , coHostDirectory :: Maybe FilePath
    , coPullRequestSpec :: PullRequestSpec
    }

data Options = Options
    { oAccessToken :: Text
    -- ^ Personal or Installation access token
    , oLogSettings :: LogSettings
    , oOwner :: Name Owner
    , oRepo :: Name Repo
    , oPullRequest :: IssueNumber
    , oJobUrl :: Maybe URL
    , oHostDirectory :: Maybe FilePath
    , oUnrestricted :: Bool
    , oStatsdHost :: Maybe String
    , oStatsdPort :: Maybe Int
    }

class HasOptions env where
    optionsL :: Lens' env Options

-- | Parse required environment variables and command-line options
--
-- See @restyler --help@
--
parseOptions :: IO Options
parseOptions = do
    EnvOptions {..} <- Env.parse id envParser
    CLIOptions {..} <-
        execParser $ info (optionsParser <**> helper) $ fullDesc <> progDesc
            "Restyle a GitHub Pull Request"

    let
        logSettings =
            maybe id adjustLogLevel eoLogLevel
                . maybe id setLogSettingsColor coColor
                $ eoLogSettings

    pure Options
        { oAccessToken = eoAccessToken
        , oLogSettings = logSettings
        , oOwner = prsOwner coPullRequestSpec
        , oRepo = prsRepo coPullRequestSpec
        , oPullRequest = prsPullRequest coPullRequestSpec
        , oJobUrl = coJobUrl
        , oHostDirectory = coHostDirectory
        , oUnrestricted = eoUnrestricted
        , oStatsdHost = eoStatsdHost
        , oStatsdPort = eoStatsdPort
        }

adjustLogLevel :: LogLevel -> LogSettings -> LogSettings
adjustLogLevel = setLogSettingsLevels . flip newLogLevels []

-- brittany-disable-next-binding

envParser :: Env.Parser Env.Error EnvOptions
envParser = EnvOptions
    <$> Env.var (Env.str <=< Env.nonempty) "GITHUB_ACCESS_TOKEN"
        (Env.help "GitHub access token with write access to the repository")
    <*> optional (Env.flag LevelInfo LevelDebug "DEBUG" Env.keep)
    <*> LoggingEnv.parser
    <*> Env.switch "UNRESTRICTED" Env.keep
    <*> optional (Env.var Env.str "STATSD_HOST" mempty)
    <*> optional (Env.var Env.auto "STATSD_PORT" mempty)

-- brittany-disable-next-binding

optionsParser :: Parser CLIOptions
optionsParser = CLIOptions
    <$> optional (option (eitherReader readLogColor)
        (  long "color"
        <> metavar "always|never|auto"
        <> help "Colorize log messages"
        <> value LogColorAuto
        ))
    <*> optional (URL <$> strOption
        (  long "job-url"
        <> metavar "URL"
        <> help "Link to Job on restyled.io"
        ))
    <*> optional (strOption
        (  long "host-directory"
        <> metavar "PATH"
        <> help "Path to host directory of sources"
        ))
    <*> argument (eitherReader parseSpec)
        (  metavar "<owner>/<name>#<number>"
        <> help "Repository and Pull Request to restyle"
        )
