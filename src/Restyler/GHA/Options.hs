{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Restyler.GHA.Options
  ( Options (..)
  , Command (..)
  , getOptions
  ) where

import Restyler.Prelude

import Data.List.NonEmpty qualified as NE
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Env qualified
import Options.Applicative
import Restyler.GitHub.Api
import Restyler.GitHub.Repository
import Restyler.GitHubTokenOption
import Restyler.LogSettingsOption
import Restyler.PullRequestOption
import Restyler.RepositoryOption
import Restyler.Restrictions

data Options = Options
  { logSettings :: LogSettingsOption
  , restrictions :: Restrictions
  , githubToken :: GitHubTokenOption
  , repository :: RepositoryOption
  , pullRequest :: PullRequestOption
  , paths :: [FilePath]
  }
  deriving stock (Generic)
  deriving (Semigroup) via (GenericSemigroupMonoid Options)

data Command
  = RestylePR GitHubToken Repository Int
  | RestylePaths (NonEmpty FilePath)

getOptions :: MonadIO m => m (Options, Command)
getOptions = do
  options <- (<>) <$> envOptions <*> optOptions

  let
    mGitHub =
      (,,)
        <$> getLast (unGitHubTokenOption options.githubToken)
        <*> getLast (unRepositoryOption options.repository)
        <*> getLast (unPullRequestOption options.pullRequest)

    mPaths = NE.nonEmpty options.paths

  case (mGitHub, mPaths) of
    (Just (token, repo, pr), Nothing) ->
      pure (options, RestylePR token repo pr)
    (Nothing, Just paths) ->
      pure (options, RestylePaths paths)
    _ -> error "TODO"

envOptions :: MonadIO m => m Options
envOptions = liftIO $ Env.parse id envParser

envParser :: Env.Parser Env.Error Options
envParser =
  Options
    <$> envLogSettingsOption
    <*> envRestrictions
    <*> envGitHubTokenOption
    <*> envRepositoryOption
    <*> envPullRequestOption
    <*> pure []

optOptions :: MonadIO m => m Options
optOptions = liftIO $ execParser $ info (optParser <**> helper) fullDesc

optParser :: Parser Options
optParser =
  Options
    <$> optLogSettingsOption
    <*> optRestrictions
    <*> optGitHubTokenOption
    <*> optRepositoryOption
    <*> optPullRequestOption
    <*> many (argument str $ metavar "PATH")
