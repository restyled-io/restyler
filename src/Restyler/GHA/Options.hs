{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Restyler.GHA.Options
  ( Options (..)
  , getOptions
  , envOptions
  , optOptions
  ) where

import Restyler.Prelude

import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Env qualified
import Options.Applicative
import Restyler.LogSettingsOption
import Restyler.PullRequestNumberOption
import Restyler.RepositoryOption
import Restyler.Restrictions

data Options = Options
  { logSettings :: LogSettingsOption
  , restrictions :: Restrictions
  , repository :: RepositoryOption
  , pullRequestNumber :: PullRequestNumberOption
  }
  deriving stock (Generic)
  deriving (Semigroup) via (GenericSemigroupMonoid Options)

getOptions :: MonadIO m => m Options
getOptions =
  (<>)
    <$> envOptions
    <*> optOptions

envOptions :: MonadIO m => m Options
envOptions = liftIO $ Env.parse id envParser

envParser :: Env.Parser Env.Error Options
envParser =
  Options
    <$> envLogSettingsOption
    <*> envRestrictions
    <*> envRepositoryOption
    <*> envPullRequestNumberOption

optOptions :: MonadIO m => m Options
optOptions = liftIO $ execParser $ info (optParser <**> helper) fullDesc

optParser :: Parser Options
optParser =
  Options
    <$> optLogSettingsOption
    <*> optRestrictions
    <*> optRepositoryOption
    <*> optPullRequestNumberOption
