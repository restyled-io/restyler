module Restyler.Options
  ( Options (..)
  , HasOptions (..)
  , parseOptions
  ) where

import Restyler.Prelude

import Blammo.Logging.LogSettings.Env qualified as LoggingEnv
import Env qualified
import GitHub.Data (IssueNumber, Owner, Repo)
import Options.Applicative
import Restyler.ManifestOption
import Restyler.PullRequestSpec
import Restyler.Restrictions

data EnvOptions = EnvOptions
  { eoAccessToken :: Text
  , eoLogSettings :: LogSettings
  , eoRepoDisabled :: Bool
  , eoPlanRestriction :: Maybe Text
  , eoPlanUpgradeUrl :: Maybe URL
  , eoRestrictions :: Restrictions
  , eoStatsdHost :: Maybe String
  , eoStatsdPort :: Maybe Int
  , eoImageCleanup :: Bool
  }

data CLIOptions = CLIOptions
  { coManifest :: Maybe FilePath
  , coJobUrl :: Maybe URL
  , coHostDirectory :: Maybe FilePath
  , coPullRequestSpec :: PullRequestSpec
  }

data Options = Options
  { oAccessToken :: Text
  -- ^ Personal or Installation access token
  , oLogSettings :: LogSettings
  , oOwner :: Name Owner
  , oRepo :: Name Repo
  , oPullRequest :: IssueNumber
  , oManifest :: Maybe FilePath
  , oJobUrl :: Maybe URL
  , oHostDirectory :: Maybe FilePath
  , oRepoDisabled :: Bool
  , oPlanRestriction :: Maybe Text
  , oPlanUpgradeUrl :: Maybe URL
  , oRestrictions :: Restrictions
  , oStatsdHost :: Maybe String
  , oStatsdPort :: Maybe Int
  , oImageCleanup :: Bool
  }

class HasOptions env where
  optionsL :: Lens' env Options

instance HasOptions Options where
  optionsL = id

instance HasManifestOption Options where
  manifestOptionL = lens (toManifestOption . oManifest) $
    \x y -> x {oManifest = unManifestOption y}

-- | Parse required environment variables and command-line options
--
-- See @restyler --help@
parseOptions :: IO Options
parseOptions = do
  EnvOptions {..} <- Env.parse id envParser
  CLIOptions {..} <-
    execParser $
      info (optionsParser <**> helper) $
        fullDesc
          <> progDesc
            "Restyle a GitHub Pull Request"

  pure
    Options
      { oAccessToken = eoAccessToken
      , oLogSettings = eoLogSettings
      , oOwner = prsOwner coPullRequestSpec
      , oRepo = prsRepo coPullRequestSpec
      , oPullRequest = prsPullRequest coPullRequestSpec
      , oManifest = coManifest
      , oJobUrl = coJobUrl
      , oHostDirectory = coHostDirectory
      , oRepoDisabled = eoRepoDisabled
      , oPlanRestriction = eoPlanRestriction
      , oPlanUpgradeUrl = eoPlanUpgradeUrl
      , oRestrictions = eoRestrictions
      , oStatsdHost = eoStatsdHost
      , oStatsdPort = eoStatsdPort
      , oImageCleanup = eoImageCleanup
      }

-- brittany-disable-next-binding

envParser :: Env.Parser Env.Error EnvOptions
envParser =
  EnvOptions
    <$> Env.var
      (Env.str <=< Env.nonempty)
      "GITHUB_ACCESS_TOKEN"
      (Env.help "GitHub access token with write access to the repository")
    <*> LoggingEnv.parser
    <*> Env.switch "REPO_DISABLED" mempty
    <*> optional (Env.var (Env.str <=< Env.nonempty) "PLAN_RESTRICTION" mempty)
    <*> optional (URL <$> Env.var (Env.str <=< Env.nonempty) "PLAN_UPGRADE_URL" mempty)
    <*> envRestrictions
    <*> optional (Env.var Env.str "STATSD_HOST" mempty)
    <*> optional (Env.var Env.auto "STATSD_PORT" mempty)
    <*> Env.switch "IMAGE_CLEANUP" mempty

-- brittany-disable-next-binding

optionsParser :: Parser CLIOptions
optionsParser =
  CLIOptions
    <$> optional
      ( strOption
          ( long "manifest"
              <> metavar "PATH"
              <> help "Local restylers manifest to use"
          )
      )
    <*> optional
      ( URL
          <$> strOption
            ( long "job-url"
                <> metavar "URL"
                <> help "Link to Job on restyled.io"
            )
      )
    <*> optional
      ( strOption
          ( long "host-directory"
              <> metavar "PATH"
              <> help "Path to host directory of sources"
          )
      )
    <*> argument
      (eitherReader parseSpec)
      ( metavar "<owner>/<name>#<number>"
          <> help "Repository and Pull Request to restyle"
      )
