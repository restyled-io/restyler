module Restyler.CLI
  ( main
  ) where

import Restyler.Prelude

import Restyler.AnnotatedException
import Restyler.App (AppT, runAppT)
import Restyler.Options.FailOnDifferences
import Restyler.RestyleResult

main
  :: (HasLogger app, HasFailOnDifferencesOption app)
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
      RestyleSuccessNoDifference -> do
        ExitSuccess <$ logInfo "No differences"
      RestyleSuccessDifference _ -> do
        failOnDifferences <- getFailOnDifferences

        if failOnDifferences
          then ExitFailure 1 <$ logError "Differences found"
          else ExitSuccess <$ logWarn "Differences found"

exitHandler :: MonadLogger m => AnnotatedException SomeException -> m ExitCode
exitHandler aex = do
  ExitFailure 1 <$ logError (displayAnnotatedException aex :# [])
