module Main
  ( main
  ) where

import Restyler.Prelude

import Restyler.CLI qualified as CLI
import Restyler.GHA
import Restyler.GHA.App
import Restyler.Options.PullRequest

main :: IO ()
main = CLI.main withApp $ do
  pr <- asks (.pullRequest)
  void $ run pr.repo pr.number
