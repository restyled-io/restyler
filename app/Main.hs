-- |
--
-- Module      : Main
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Main
  ( main
  ) where

import Restyler.Prelude

import Data.Aeson
import Restyler.App
import Restyler.CLI qualified as CLI
import Restyler.Config
import Restyler.GitHub.PullRequest
import Restyler.Options
import Restyler.Restyle qualified as Restyle

main :: IO ()
main = CLI.main withApp $ do
  config <- asks (.config)

  let
    paths = toList config.options.paths
    mJSON = config.options.pullRequestJson

  case mJSON of
    Nothing -> Restyle.run config NullPullRequest paths
    Just path -> do
      result <- liftIO $ eitherDecodeFileStrict @PullRequest $ toFilePath path
      case result of
        Left err -> do
          logError $ ("pull-request-json is invalid:\n" <> pack err) :# []
          exitFailure
        Right pr -> Restyle.run config pr paths
