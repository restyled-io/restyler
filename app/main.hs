module Main
  ( main
  ) where

import Restyler.Prelude

import Restyler.CLI qualified as CLI
import Restyler.Job
import Restyler.Job.App

main :: IO ()
main = CLI.main withApp $ do
  jobUrl <- asks (.jobUrl)
  pr <- asks (.pullRequest)
  run jobUrl pr
