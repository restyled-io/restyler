{-# LANGUAGE LambdaCase #-}

module Restyler.Options
    ( Options(..)
    , HasOptions(..)
    , parseOptions
    )
where

import Restyler.Prelude

import qualified Env
import Options.Applicative
import Restyler.PullRequestSpec
import System.Console.ANSI (hSupportsANSI)
import System.IO (stderr, stdout)

data ColorOption
    = AlwaysColor
    | NeverColor
    | AutoColor

data EnvOptions = EnvOptions
    { eoAccessToken :: Text
    , eoLogLevel :: LogLevel
    }

data CLIOptions = CLIOptions
    { coColor :: ColorOption
    , coJobUrl :: Maybe URL
    , coPullRequestSpec :: PullRequestSpec
    }

data Options = Options
    { oAccessToken :: Text
    -- ^ Personal or Installation access token
    , oLogLevel :: LogLevel
    , oLogColor :: Bool
    , oOwner :: Name Owner
    , oRepo :: Name Repo
    , oPullRequest :: IssueNumber
    , oJobUrl :: Maybe URL
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

    logColor <- case coColor of
        AlwaysColor -> pure True
        NeverColor -> pure False
        AutoColor -> and <$> traverse hSupportsANSI [stdout, stderr]

    pure Options
        { oAccessToken = eoAccessToken
        , oLogLevel = eoLogLevel
        , oLogColor = logColor
        , oOwner = prsOwner coPullRequestSpec
        , oRepo = prsRepo coPullRequestSpec
        , oPullRequest = prsPullRequest coPullRequestSpec
        , oJobUrl = coJobUrl
        }

-- brittany-disable-next-binding
envParser :: Env.Parser Env.Error EnvOptions
envParser = EnvOptions
    <$> Env.var (Env.str <=< Env.nonempty) "GITHUB_ACCESS_TOKEN"
        (Env.help "GitHub access token with write access to the repository")
    <*> Env.flag LevelInfo LevelDebug "DEBUG" Env.keep

-- brittany-disable-next-binding
optionsParser :: Parser CLIOptions
optionsParser = CLIOptions
    <$> option (eitherReader parseColorOption)
        (  long "color"
        <> metavar "always|never|auto"
        <> help "Colorize log messages"
        <> value AutoColor
        )
    <*> optional (URL <$> strOption
        (  long "job-url"
        <> metavar "URL"
        <> help "Link to Job on restyled.io"
        ))
    <*> argument (eitherReader parseSpec)
        (  metavar "<owner>/<name>#<number>"
        <> help "Repository and Pull Request to restyle"
        )

parseColorOption :: String -> Either String ColorOption
parseColorOption = \case
    "always" -> Right AlwaysColor
    "never" -> Right NeverColor
    "auto" -> Right AutoColor
    x -> Left $ "Invalid color option: " <> x
