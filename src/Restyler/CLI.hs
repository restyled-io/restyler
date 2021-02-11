module Restyler.CLI
    ( restylerCLI
    )
where

import Restyler.Prelude

import Restyler.App.Error
import Restyler.Options (Options)
import System.IO (hPutStrLn)

restylerCLI
    :: IO Options -> (Options -> FilePath -> IO (Either AppError a)) -> IO a
restylerCLI parseOptions f = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    options <- parseOptions
    either die pure =<< withSystemTempDirectory "restyler-" (f options)

die :: AppError -> IO a
die e = do
    hPutStrLn stderr $ prettyAppError e
    exitWith $ ExitFailure $ edExitCode $ appErrorDetails e
