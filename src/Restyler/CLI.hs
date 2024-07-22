module Restyler.CLI
  ( main
  ) where

import Restyler.Prelude

import Restyler.AnnotatedException
import Restyler.App (AppT, runAppT)
import Restyler.RestyleResult

main
  :: HasLogger app
  => (forall a. (app -> IO a) -> IO a)
  -> AppT app IO (RestyleResult pr)
  -> IO ()
main withApp run = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  exitWith =<< withApp (`runAppT` (go `catch` exitHandler))
 where
  go = do
    result <- run

    let
      message :: Message
      message = case result of
        RestyleSkipped _ _ reason -> "Restyle skipped" :# ["reason" .= reason]
        RestyleSuccessNoDifference {} -> "No differences"
        RestyleSuccessDifference {} -> "Differences found"

    ExitSuccess <$ logInfo message

exitHandler :: MonadLogger m => AnnotatedException SomeException -> m ExitCode
exitHandler aex = do
  ExitFailure 1 <$ logError (displayAnnotatedException aex :# [])
