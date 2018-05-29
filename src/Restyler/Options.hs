{-# LANGUAGE RecordWildCards #-}

module Restyler.Options
    ( Options(..)
    , parseOptions
    )
where

import Restyler.Prelude

import qualified Env
import GitHub.Data
import Options.Applicative
import Restyler.RepoSpec

data Options = Options
    { oAccessToken :: Text
    -- ^ Personal or Installation access token
    , oLogLevel :: LogLevel
    , oOwner :: Name Owner
    , oRepo :: Name Repo
    , oPullRequest :: Int
    , oJobUrl :: Maybe URL
    }

-- | Parse required environment variables and command-line options
--
-- Environment variables:
--
-- - @DEBUG=1@
-- - @GITHUB_ACCESS_TOKEN=@ (required)
--
-- Usage:
--
-- > restyler [--job-url <url>@] <owner>/<repo>#<number>
--
parseOptions :: IO Options
parseOptions = do
    (accessToken, logLevel) <- Env.parse id envParser
    (mJobUrl, RepoSpec {..}) <-
        execParser $ info (optionsParser <**> helper) $ fullDesc <> progDesc
            "Restyle a GitHub Pull Request"

    pure Options
        { oAccessToken = accessToken
        , oLogLevel = logLevel
        , oOwner = rsOwner
        , oRepo = rsRepo
        , oPullRequest = rsPullRequest
        , oJobUrl = mJobUrl
        }

-- brittany-disable-next-binding
envParser :: Env.Parser Env.Error (Text, LogLevel)
envParser = (,)
    <$> Env.var (Env.str <=< Env.nonempty) "GITHUB_ACCESS_TOKEN"
        (Env.help "GitHub access token with write access to the repository")
    <*> Env.flag LevelInfo LevelDebug "DEBUG" Env.keep

-- brittany-disable-next-binding
optionsParser :: Parser (Maybe URL, RepoSpec)
optionsParser = (,)
    <$> optional (URL <$> strOption
        (  long "job-url"
        <> metavar "URL"
        <> help "Link to Job on restyled.io"
        ))
    <*> argument (eitherReader parseRepoSpec)
        (  metavar "<owner>/<name>#<number>"
        <> help "Repository and Pull Request to restyle"
        )
