{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Restyler.Options.RestyleGHA
  ( Options (..)
  , getOptions
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative
import Restyler.GHA
import Restyler.GitHub.Api (GitHubToken, HasGitHubToken (..), envGitHubToken)
import Restyler.HostDirectoryOption
import Restyler.LogSettingsOption
import Restyler.Restrictions

data Options = Options
  { logSettings :: LogSettings
  , githubToken :: GitHubToken
  , githubOutput :: GitHubOutput
  , restrictions :: Restrictions
  , hostDirectory :: HostDirectoryOption
  }

instance HasGitHubToken Options where
  githubTokenL = lens (.githubToken) $ \x y -> x {githubToken = y}

instance HasGitHubOutput Options where
  githubOutputL = lens (.githubOutput) $ \x y -> x {githubOutput = y}

instance HasRestrictions Options where
  restrictionsL = lens (.restrictions) $ \x y -> x {restrictions = y}

instance HasHostDirectoryOption Options where
  hostDirectoryOptionL = lens (.hostDirectory) $ \x y -> x {hostDirectory = y}

getOptions :: IO Options
getOptions = do
  env <- envOptions
  opt <- optOptions

  pure
    $ Options
      { logSettings = resolveLogSettings $ env.logSettings <> opt.logSettings
      , githubToken = env.githubToken
      , githubOutput = env.githubOutput
      , restrictions = env.restrictions <> opt.restrictions
      , hostDirectory = env.hostDirectory <> opt.hostDirectory
      }

-- | Options as read from the environment
data EnvOptions = EnvOptions
  { githubToken :: GitHubToken
  , githubOutput :: GitHubOutput
  , logSettings :: LogSettingsOption
  , restrictions :: Restrictions
  , hostDirectory :: HostDirectoryOption
  }

envOptions :: IO EnvOptions
envOptions =
  Env.parse id
    $ EnvOptions
    <$> envGitHubToken
    <*> envGitHubOutput
    <*> envLogSettingsOption
    <*> envRestrictions
    <*> envHostDirectoryOption

-- | Options as read via the CLI
data OptOptions = OptOptions
  { logSettings :: LogSettingsOption
  , restrictions :: Restrictions
  , hostDirectory :: HostDirectoryOption
  }

optOptions :: IO OptOptions
optOptions = execParser $ info (p <**> helper) fullDesc
 where
  p =
    OptOptions
      <$> optLogSettingsOption
      <*> optRestrictions
      <*> optHostDirectoryOption
