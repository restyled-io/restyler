{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Restyler.Options.RestyleGHA
  ( Options (..)
  , getOptions
  ) where

import Restyler.Prelude

import Env qualified
import Options.Applicative
import Restyler.GitHub.Api (GitHubToken, HasGitHubToken (..), envGitHubToken)
import Restyler.LogSettingsOption

data Options = Options
  { logSettings :: LogSettings
  , githubToken :: GitHubToken
  }

instance HasGitHubToken Options where
  githubTokenL = lens (.githubToken) $ \x y -> x {githubToken = y}

getOptions :: MonadIO m => m Options
getOptions = liftIO $ do
  env <- envOptions
  opt <- optOptions

  pure
    $ Options
      { logSettings = resolveLogSettings $ env.logSettings <> opt.logSettings
      , githubToken = env.githubToken
      }

-- | Options as read from the environment
data EnvOptions = EnvOptions
  { githubToken :: GitHubToken
  , logSettings :: LogSettingsOption
  }

envOptions :: IO EnvOptions
envOptions =
  Env.parse id
    $ EnvOptions
    <$> envGitHubToken
    <*> envLogSettingsOption

-- | Options as read via the CLI
newtype OptOptions = OptOptions
  { logSettings :: LogSettingsOption
  }

optOptions :: IO OptOptions
optOptions = execParser $ info (p <**> helper) fullDesc
 where
  p = OptOptions <$> optLogSettingsOption
