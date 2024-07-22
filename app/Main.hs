module Main
  ( main
  ) where

import Restyler.Prelude

import Restyler.CLI qualified as CLI
import Restyler.Local
import Restyler.Local.App

main :: IO ()
main = CLI.main withApp $ do
  paths <- asks (.paths)
  run NullPullRequest $ toList paths
