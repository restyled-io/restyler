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
import Restyler.Restyle qualified as Restyle

main :: IO ()
main = CLI.main withApp $ do
  paths <- toList <$> asks (.config.paths)
  mJSON <- asks (.config.pullRequestJson)

  case mJSON of
    Nothing -> Restyle.run NullPullRequest paths
    Just path -> do
      result <- liftIO $ eitherDecodeFileStrict @PullRequest $ toFilePath path
      case result of
        Left err -> do
          logError $ ("pull-request-json is invalid:\n" <> pack err) :# []
          exitFailure
        Right pr -> Restyle.run pr paths
