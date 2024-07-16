module Restyler.CLI
  ( main
  ) where

import Restyler.Prelude

import Restyler.App (AppT, runAppT)
import Restyler.ErrorMetadata
import UnliftIO.Exception (catchAny)

main
  :: HasLogger app
  => ((app -> IO a) -> IO b)
  -> AppT app IO a
  -> IO b
main withApp run = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  withApp $ \app -> do
    runAppT app $ do
      run `catchAny` \ex -> do
        logErrorMetadataAndExit $ errorMetadata ex
