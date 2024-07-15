module Main
  ( main
  ) where

import Restyler.Prelude

import Restyler.Command
import Restyler.Commands.RestyleGHA qualified as RestyleGHA
import Restyler.Commands.RestyleLocal qualified as RestyleLocal

main :: IO ()
main = do
  -- Just incase we're run via Docker
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  cmd <- getCommand

  case cmd of
    RestyleJob {} -> pure () -- TODO
    RestyleGHA env options repo number -> RestyleGHA.main env options repo number
    RestyleLocal options paths -> RestyleLocal.main options paths
