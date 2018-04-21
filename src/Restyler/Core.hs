{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Core
    ( restyle
    ) where

import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Foldable (traverse_)
import Data.List (isPrefixOf)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Restyler.Config
import Restyler.Core.App
import System.Directory (getCurrentDirectory)
import System.Exit hiding (die)
import System.Process (callProcess)

restyle :: [FilePath] -> IO ()
restyle paths = do
    app <- loadApp

    runApp app $ do
        config <- either (die . T.pack) pure =<< liftIO loadConfig
        logDebugN $ "Loaded config: " <> tshow config

        unless (cEnabled config) $ do
            logWarnN "Restyler disabled by config"
            liftIO exitSuccess

        logDebugN $ "Paths: " <> tshow paths
        traverse_ (callRestyler paths) $ cRestylers config

callRestyler :: [FilePath] -> Restyler -> AppM ()
callRestyler allPaths r = do
    cwd <- liftIO getCurrentDirectory
    paths <- liftIO $ restylePaths r allPaths

    unless (null paths) $ do
        logInfoN $ "Restyling " <> tshow paths <> " via " <> T.pack (rName r)
        logDebugN $ tshow $ "docker" : dockerArguments cwd r paths
        liftIO $ callProcess "docker" $ dockerArguments cwd r paths

dockerArguments :: FilePath -> Restyler -> [FilePath] -> [String]
dockerArguments dir Restyler {..} paths =
    [ "run"
        , "--rm"
        , "--volume"
        , dir <> ":/code"
        , "--net"
        , "none"
        , "restyled/restyler-" <> rName
        , rCommand
        ]
        ++ rArguments
        ++ prependArgSep (map prependIfRelative paths)
  where
    prependArgSep
        | rSupportsArgSep = ("--" :)
        | otherwise = id

    prependIfRelative path
        | any (`isPrefixOf` path) ["/", "./", "../"] = path
        | otherwise = "./" <> path

tshow :: Show a => a -> Text
tshow = T.pack . show
