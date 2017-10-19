{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module Restyler.Options
    ( Options(..)
    , parseOptions
    , optionsToFlags
    ) where

import ClassyPrelude

import Data.Proxy
import GitHub.Data
import GitHub.Data.Apps
import Options.Applicative

data Options = Options
    { oGitHubAppId :: Id App
    , oGitHubAppKey :: Text
    , oInstallationId :: Id Installation
    , oOwner :: Name Owner
    , oRepo :: Name Repo
    , oPullRequest :: Id PullRequest
    , oRestyledRoot :: Text
    }

options :: Parser Options
options = Options
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

parseOptions :: IO Options
parseOptions = execParser $ info (options <**> helper)
    (fullDesc <> progDesc "Restyle a GitHub Pull Request")

-- | Generate CLI flags
--
-- Library call-sites (e.g. @"Backend"@) should build type-safe @'Options'@ then
-- use this. The function is co-located here as a best-effort way to avoid
-- breaking things at this un-typed boundary.
--
optionsToFlags :: Options -> [String]
optionsToFlags Options{..} =
    [ "--github-app-id", unpack $ toPathPart oGitHubAppId
    , "--github-app-key", unpack oGitHubAppKey
    , "--installation-id", unpack $ toPathPart oInstallationId
    , "--owner", unpack $ toPathPart oOwner
    , "--repo", unpack $ toPathPart oRepo
    , "--pull-request", unpack $ toPathPart oPullRequest
    , "--restyled-root", unpack oRestyledRoot
    ]
