-- |
--
-- Module      : Restyler.Options
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options
  ( Options
  , getLogSettings
  , getPullRequestJson
  , getConfigPath
  , getPaths
  , envParser
  , optParser
  , module X
  ) where

import Restyler.Prelude

import Data.List.NonEmpty (some1)
import Data.Semigroup.Generic
import Env qualified
import Options.Applicative
import Restyler.Option as X
import Restyler.Options.DryRun as X
import Restyler.Options.FailOnDifferences as X
import Restyler.Options.HostDirectory as X
import Restyler.Options.ImageCleanup as X
import Restyler.Options.LogSettings as X
import Restyler.Options.Manifest as X
import Restyler.Options.NoClean as X
import Restyler.Options.NoCommit as X
import Restyler.Options.NoPull as X
import Restyler.Options.Restrictions as X

data Options = Options
  { dryRun :: Maybe (Option DryRun Bool)
  , failOnDifferences :: Maybe (Option FailOnDifferences Bool)
  , hostDirectory :: Maybe (Option HostDirectory FilePath)
  , imageCleanup :: Maybe (Option ImageCleanup Bool)
  , manifest :: Maybe (Option Manifest FilePath)
  , noCommit :: Maybe (Option NoCommit Bool)
  , noClean :: Maybe (Option NoClean Bool)
  , noPull :: Maybe (Option NoPull Bool)
  , unrestricted :: Maybe (Option Unrestricted Bool)
  , restylerNoNetNone :: Maybe (Option RestylerNoNetNone Bool)
  , restylerCpuShares :: Maybe (Option RestylerCpuShares Natural)
  , restylerMemory :: Maybe (Option RestylerMemory Bytes)
  , logSettings :: Maybe LogSettingsOption
  , pullRequestJson :: Maybe FilePath
  , configPath :: Maybe FilePath
  , paths :: Maybe (NonEmpty FilePath)
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)
  deriving (Semigroup, Monoid) via (GenericSemigroupMonoid Options)

instance HasOption DryRun Options Bool where
  getOption = (.dryRun)

instance HasOption FailOnDifferences Options Bool where
  getOption = (.failOnDifferences)

instance HasOption HostDirectory Options FilePath where
  getOption = (.hostDirectory)

instance HasOption ImageCleanup Options Bool where
  getOption = (.imageCleanup)

instance HasOption Manifest Options FilePath where
  getOption = (.manifest)

instance HasOption NoCommit Options Bool where
  getOption = (.noCommit)

instance HasOption NoClean Options Bool where
  getOption = (.noClean)

instance HasOption NoPull Options Bool where
  getOption = (.noPull)

instance HasOption Unrestricted Options Bool where
  getOption = (.unrestricted)

instance HasOption RestylerNoNetNone Options Bool where
  getOption = (.restylerNoNetNone)

instance HasOption RestylerCpuShares Options Natural where
  getOption = (.restylerCpuShares)

instance HasOption RestylerMemory Options Bytes where
  getOption = (.restylerMemory)

getLogSettings :: Options -> LogSettings
getLogSettings = resolveLogSettings . fromMaybe mempty . (.logSettings)

getPullRequestJson :: Options -> Maybe FilePath
getPullRequestJson = (.pullRequestJson)

getConfigPath :: Options -> Maybe FilePath
getConfigPath = (.configPath)

getPaths :: Options -> [FilePath]
getPaths = maybe [] toList . (.paths)

envParser :: Env.Parser Env.Error Options
envParser =
  Options
    <$> envOption dryRunSpec
    <*> envOption failOnDifferencesSpec
    <*> envOption hostDirectorySpec
    <*> envOption imageCleanupSpec
    <*> envOption manifestSpec
    <*> envOption noCommitSpec
    <*> envOption noCleanSpec
    <*> envOption noPullSpec
    <*> envOption unrestrictedSpec
    <*> envOption restylerNoNetNoneSpec
    <*> envOption restylerCpuSharesSpec
    <*> envOption restylerMemorySpec
    <*> optional envLogSettingsOption
    <*> optional (Env.var Env.nonempty "PULL_REQUEST_JSON" mempty)
    <*> optional (Env.var Env.nonempty "CONFIG" $ Env.help configHelp)
    <*> pure Nothing

optParser :: Parser Options
optParser =
  Options
    <$> optOption dryRunSpec
    <*> optOption failOnDifferencesSpec
    <*> optOption hostDirectorySpec
    <*> optOption imageCleanupSpec
    <*> optOption manifestSpec
    <*> optOption noCommitSpec
    <*> optOption noCleanSpec
    <*> optOption noPullSpec
    <*> optOption unrestrictedSpec
    <*> optOption restylerNoNetNoneSpec
    <*> optOption restylerCpuSharesSpec
    <*> optOption restylerMemorySpec
    <*> optional optLogSettingsOption
    <*> optional (option str $ long "pull-request-json" <> hidden)
    <*> optional (option str $ long "config" <> metavar "PATH" <> help configHelp)
    <*> optional (some1 $ argument str $ metavar "PATH")

configHelp :: String
configHelp = "Path to configuration file"
