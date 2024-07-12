module Restyler.GHA.Options
  ( Options (..)
  , parseOptions
  ) where

import Restyler.Prelude

import Blammo.Logging.LogSettings.Env qualified as LogSettingsEnv
import Env qualified

data Options = Options
  { logSettings :: LogSettings
  , githubEventJson :: FilePath
  }

parseOptions :: MonadIO m => m Options
parseOptions = liftIO $ Env.parse id parser

parser :: Env.Parser Env.Error Options
parser =
  Options
    <$> LogSettingsEnv.parser
    <*> Env.var Env.nonempty "GITHUB_EVENT_JSON" mempty
