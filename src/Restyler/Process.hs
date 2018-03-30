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

    when (exitCode /= ExitSuccess) $ die $ unlines
        [ "Process unsuccessful"
        , "Command: " <> cmd
        , "Arguments: " <> show args
        , "stdout:\n" <> indent stdout
        , "stderr:\n" <> indent stderr
        ]
  where
    indent = unlines . map ("  " <>) . lines
