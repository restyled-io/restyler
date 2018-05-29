{-# LANGUAGE OverloadedStrings #-}

module Restyler.Process
    ( callProcess
    , readProcess
    , readProcessMay
    )
where

import Restyler.Prelude.NoApp

import Restyler.App
import qualified System.Process as Process

callProcess :: String -> [String] -> AppM ()
callProcess cmd args = do
    logDebugN $ "callProcess: " <> tshow (cmd : args)
    liftIOApp $ Process.callProcess cmd args

readProcess :: String -> [String] -> String -> AppM String
readProcess cmd args stdin = do
    logDebugN $ "readProcess: " <> tshow (cmd : args)
    liftIOApp $ Process.readProcess cmd args stdin

readProcessMay :: String -> [String] -> String -> AppM (Maybe String)
readProcessMay cmd args = hushM . readProcess cmd args
