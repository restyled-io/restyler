module Restyler.Process
    ( callProcess
    ) where

import Control.Monad (when)
import Data.Monoid ((<>))
import System.Exit (ExitCode(..), die)
import System.Process (readProcessWithExitCode)

callProcess :: String -> [String] -> IO ()
callProcess cmd args = do
    (exitCode, stdout, stderr) <- readProcessWithExitCode cmd args ""

    when (exitCode /= ExitSuccess) $ die $ unlines $
        [ "Process unsuccessful ("  <> show exitCode <> ")"
        , "  command: " <> cmd
        , "  arguments: " <> show args
        ]
        ++ "  stdout:" : indent stdout
        ++ "  stderr:" : indent stderr
  where
    indent = map ("    " <>) . lines
