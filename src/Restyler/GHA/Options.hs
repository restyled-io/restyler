{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Restyler.GHA.Options
  ( Options (..)
  , parseOptions
  ) where

import Restyler.Prelude

import Blammo.Logging.LogSettings.Env qualified as LogSettingsEnv
import Env qualified
import Restyler.HostDirectoryOption
import Restyler.Restrictions

data Options = Options
  { logSettings :: LogSettings
  , hostDirectory :: FilePath
  , restrictions :: Restrictions
  , githubEventJson :: FilePath
  , githubPRFilesJson :: FilePath
  }

instance HasHostDirectoryOption Options where
  hostDirectoryOptionL = lens
    (toHostDirectoryOption . Just . (.hostDirectory))
    $ \x y -> case unHostDirectoryOption y of
      Nothing -> x
      Just y' -> x {hostDirectory = y'}

instance HasRestrictions Options where
  restrictionsL = lens (.restrictions) $ \x y -> x {restrictions = y}

parseOptions :: MonadIO m => m Options
parseOptions = liftIO $ Env.parse id parser

parser :: Env.Parser Env.Error Options
parser =
  Options
    <$> LogSettingsEnv.parser
    <*> Env.var Env.nonempty "HOST_DIRECTORY" mempty
    <*> envRestrictions
    <*> Env.var Env.nonempty "GITHUB_EVENT_JSON" mempty
    <*> Env.var Env.nonempty "GITHUB_PR_FILES_JSON" mempty
