{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Options
    ( Options(..)
    , parseOptions
    ) where

import ClassyPrelude

import Data.Proxy
import qualified Env
import GitHub.Data
import GitHub.Data.Apps
import GitHub.Endpoints.Installations
import Options.Applicative
import Restyler.RepoSpec
import System.Environment (lookupEnv)

data Options = Options
    { oAccessToken :: Text
    , oOwner :: Name Owner
    , oRepo :: Name Repo
    , oPullRequest :: Id PullRequest
    }

parseRealOptions :: IO Options
parseRealOptions = do
    accessToken <- Env.parse id
        $ Env.var (Env.str <=< Env.nonempty) "GITHUB_ACCESS_TOKEN"
        $ Env.help "GitHub access token with write access to the repository"

    RepoSpec{..} <- execParser
        $ info (optionsParser <**> helper)
        $ fullDesc <> progDesc "Restyle a GitHub Pull Request"

    pure Options
        { oAccessToken = accessToken
        , oOwner = rsOwner
        , oRepo = rsRepo
        , oPullRequest = rsPullRequest
        }
  where
    optionsParser :: Parser RepoSpec
    optionsParser = argument (eitherReader parseRepoSpec)
        (  metavar "<owner>/<name>#<number>"
        <> help "Repository and Pull Request to restyle"
        )

--------------------------------------------------------------------------------
-- Beyond here will go away, after we update the Backend caller
--------------------------------------------------------------------------------
parseOptions :: IO Options
parseOptions = do
    mToken <- lookupEnv "GITHUB_ACCESS_TOKEN"

    case mToken of
        Just _ -> parseRealOptions
        _ -> parseLegacyOptions

data Options' = Options'
    { oGitHubAppId' :: Id App
    , oGitHubAppKey' :: Text
    , oInstallationId' :: Id Installation
    , oOwner' :: Name Owner
    , oRepo' :: Name Repo
    , oPullRequest' :: Id PullRequest
    , oRestyledRoot' :: Text
    }

parseLegacyOptions :: IO Options
parseLegacyOptions = do
    Options'{..} <- parseOptions'
    accessToken <- createAccessToken
        oGitHubAppId'
        oGitHubAppKey'
        oInstallationId'

    pure Options
        { oAccessToken = atToken accessToken
        , oOwner = oOwner'
        , oRepo = oRepo'
        , oPullRequest = oPullRequest'
        }

options :: Parser Options'
options = Options'
    <$> (mkId Proxy <$> option auto
        (  long "github-app-id"
        <> metavar "ID"
        <> help "GitHub App Id"
        ))
    <*> (pack <$> strOption
        (  long "github-app-key"
        <> metavar "KEY"
        <> help "GitHub App Key"
        ))
    <*> (mkId Proxy <$> option auto
        (  long "installation-id"
        <> metavar "ID"
        <> help "Installation Id"
        ))
    <*> (mkName Proxy . pack <$> strOption
        (  long "owner"
        <> metavar "NAME"
        <> help "Owner"
        ))
    <*> (mkName Proxy . pack <$> strOption
        (  long "repo"
        <> metavar "NAME"
        <> help "Repo"
        ))
    <*> (mkId Proxy <$> option auto
        (  long "pull-request"
        <> metavar "NUMBER"
        <> help "Pull Request"
        ))
    <*> (pack <$> strOption
        (  long "restyled-root"
        <> metavar "URL"
        <> help "Root for restyled.io"
        <> value "https://restyled.io"
        ))

parseOptions' :: IO Options'
parseOptions' = execParser $ info (options <**> helper)
    (fullDesc <> progDesc "Restyle a GitHub Pull Request")
