module Main
  ( main
  ) where

import Restyler.Prelude

import Restyler.Command
import Restyler.Commands.RestyleGHA qualified as RestyleGHA

main :: IO ()
main = do
  cmd <- getCommand

  case cmd of
    RestyleGHA repo number -> RestyleGHA.run repo number
