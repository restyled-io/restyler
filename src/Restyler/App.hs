{-# LANGUAGE RecordWildCards #-}
module Restyler.App
    ( App(..)
    , AppM
    , runApp
    , loadApp
    , module Control.Monad.Logger
    , module Control.Monad.Reader
    ) where

import Control.Monad.Logger
import Control.Monad.Reader
import qualified Env
import System.IO

data App c = App
    { appConfig :: c
    , appLogLevel :: LogLevel
    }

type AppM c = ReaderT (App c) (LoggingT IO)

loadApp :: c -> IO (App c)
loadApp c =
    Env.parse id
        $ App
        <$> pure c
        <*> Env.flag LevelInfo LevelDebug "DEBUG" Env.keep

runApp :: App c -> AppM c a -> IO a
runApp app@App {..} action = do
    -- Ensure output alwyas works correctly in Docker
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    runStdoutLoggingT
        $ filterLogger (\_ level -> level >= appLogLevel)
        $ runReaderT action app
