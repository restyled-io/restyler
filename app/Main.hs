module Main
  ( main
  ) where

import Restyler.Prelude

import Data.Aeson
import Restyler.CLI qualified as CLI
import Restyler.GitHub.PullRequest
import Restyler.Local
import Restyler.Local.App

main :: IO ()
main = CLI.main withApp $ do
  paths <- asks (.paths)
  mJSON <- asks (.pullRequestJson)
  case mJSON of
    Nothing -> run NullPullRequest $ toList paths
    Just path -> do
      result <- liftIO $ eitherDecodeFileStrict @PullRequest path
      case result of
        Left err -> do
          logError $ ("pull-request-json is invalid:\n" <> pack err) :# []
          exitFailure
        Right pr -> run pr $ toList paths
