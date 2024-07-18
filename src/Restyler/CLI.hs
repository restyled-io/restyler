module Restyler.CLI
  ( main
  ) where

import Restyler.Prelude

import Restyler.App (AppT, runAppT)
import Restyler.ErrorMetadata
import Restyler.RestyleResult
import UnliftIO.Exception (handleAny)

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
      handleAny (logErrorMetadataAndExit . errorMetadata) $ do
        result <- run
        case result of
          RestyleSkipped _ _ reason -> logInfo $ "Restyle skipped" :# ["reason" .= reason]
          RestyleSuccessNoDifference {} -> logInfo "No differences"
          RestyleSuccessDifference {} -> logInfo "Differences found"
