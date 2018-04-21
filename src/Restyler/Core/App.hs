{-# LANGUAGE RecordWildCards #-}

module Restyler.Core.App
    ( App(..)
    , AppM
    , loadApp
    , runApp
    , die
    ) where

import Control.Monad.Logger
import Control.Monad.Reader
import Data.Text (Text)
import qualified Env
import System.Exit hiding (die)
import System.IO

newtype App = App
    { appLogLevel :: LogLevel
    }

type AppM = ReaderT App (LoggingT IO)

loadApp :: IO App
loadApp = Env.parse id $ App
    <$> Env.flag LevelInfo LevelDebug "DEBUG" mempty

runApp :: App -> AppM a -> IO a
runApp app@App{..} action = do
  -- Ensure output is streamed if in a Docker container
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  runStdoutLoggingT
    $ filterLogger (\_ level -> level >= appLogLevel)
    $ runReaderT action app

die :: Text -> AppM a
die msg = do
    logErrorN msg
    liftIO exitFailure
