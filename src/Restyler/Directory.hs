{-# LANGUAGE OverloadedStrings #-}

module Restyler.Directory
    ( doesFileExist
    , getCurrentDirectory
    , setCurrentDirectory
    )
where

import Restyler.Prelude.NoApp

import Restyler.App
import qualified System.Directory as Directory

doesFileExist :: FilePath -> AppM Bool
doesFileExist path = do
    logDebugN $ "doesFileExist: " <> tshow path
    liftIOApp $ Directory.doesFileExist path

getCurrentDirectory :: AppM FilePath
getCurrentDirectory = do
    logDebugN "getCurrentDirectory"
    liftIOApp Directory.getCurrentDirectory

setCurrentDirectory :: FilePath -> AppM ()
setCurrentDirectory path = do
    logDebugN "setCurrentDirectory"
    liftIOApp $ Directory.setCurrentDirectory path
