module Restyler.CLI
  ( main
  ) where

import Restyler.Prelude

import Restyler.App (AppT, runAppT)
import Restyler.ErrorMetadata
import Restyler.RestyleResult

main
  :: HasLogger app
  => ((app -> IO ()) -> IO a)
  -> AppT app IO (RestyleResult pr)
  -> IO a
main withApp run = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  withApp $ \app -> do
    runAppT app $ do
      result <- run
      case result of
        RestyleFailedEarly ex -> logErrorMetadataAndExit $ errorMetadata ex
        RestyleFailed _ _ ex -> logErrorMetadataAndExit $ errorMetadata ex
        RestyleSkipped _ _ reason -> logInfo $ "Restyle skipped" :# ["reason" .= reason]
        RestyleSuccessNoDifference {} -> logInfo "No differences"
        RestyleSuccessDifference {} -> logInfo "Differences found"
