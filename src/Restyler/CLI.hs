-- |
--
-- Module      : Restyler.CLI
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.CLI
  ( main
  ) where

import Restyler.Prelude

import Restyler.AnnotatedException
import Restyler.App (AppT, runAppT)
import Restyler.Config.FailOnDifferences
import Restyler.RestyleResult

main
  :: (HasFailOnDifferences app, HasLogger app)
  => (forall a. (app -> IO a) -> IO a)
  -> AppT app IO RestyleResult
  -> IO ()
main withApp run = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  exitWith =<< withApp (`runAppT` (go `catch` exitHandler))
 where
  go = do
    result <- run

    case result of
      RestyleSkipped reason -> do
        logInfo $ "Restyle skipped" :# ["reason" .= reason]
        pure ExitSuccess
      RestyleNoDifference -> do
        ExitSuccess <$ logInfo "No differences"
      RestyleDifference -> do
        failOnDifferences <- asks getFailOnDifferences

        if failOnDifferences
          then ExitFailure 228 <$ logError "Differences found"
          else ExitSuccess <$ logWarn "Differences found"

exitHandler :: MonadLogger m => AnnotatedException SomeException -> m ExitCode
exitHandler aex = do
  ExitFailure 1 <$ logError (displayAnnotatedException aex :# [])
