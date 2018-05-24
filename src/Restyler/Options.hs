{-# LANGUAGE RecordWildCards #-}

module Restyler.Options
    ( Options(..)
    , parseOptions
    ) where

import Restyler.Prelude

import qualified Env
import GitHub.Data
import Options.Applicative
import Restyler.RepoSpec

data Options = Options
    { oAccessToken :: Text
    , oOwner :: Name Owner
    , oRepo :: Name Repo
    , oPullRequest :: Id PullRequest
    , oJobUrl :: Maybe URL
    }

parseOptions :: IO Options
parseOptions = do
    accessToken <-
        Env.parse id
        $ Env.var (Env.str <=< Env.nonempty) "GITHUB_ACCESS_TOKEN"
        $ Env.help "GitHub access token with write access to the repository"

    (mJobUrl, RepoSpec {..}) <-
        execParser $ info (optionsParser <**> helper) $ fullDesc <> progDesc
            "Restyle a GitHub Pull Request"

    pure Options
        { oAccessToken = accessToken
        , oOwner = rsOwner
        , oRepo = rsRepo
        , oPullRequest = rsPullRequest
        , oJobUrl = mJobUrl
        }

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
