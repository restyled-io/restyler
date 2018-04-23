{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Core
    ( restyle
    ) where

import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Foldable (for_)
import Data.List (isPrefixOf)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Restyler.App
import Restyler.Config
import System.Directory (getCurrentDirectory)
import System.Exit (die, exitSuccess)
import System.Process (callProcess)

restyle :: [FilePath] -> IO ()
restyle paths = do
    app <- loadApp =<< either (die . show) pure =<< loadConfig

    runApp app $ do
        config <- asks appConfig
        logDebugN $ "Loaded config: " <> tshow config

        unless (cEnabled config) $ do
            logWarnN "Restyler disabled by config"
            liftIO exitSuccess

        logDebugN $ "Paths: " <> tshow paths
        callRestylers paths

callRestylers :: [FilePath] -> AppM Config ()
callRestylers allPaths = do
    cwd <- liftIO getCurrentDirectory
    restylers <- asks $ cRestylers . appConfig

    for_ restylers $ \r@Restyler{..} -> do
        paths <- liftIO $ restylePaths r allPaths
        unless (null paths) $ do
            logInfoN $ "Restyling " <> tshow paths <> " via " <> T.pack rName
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
