{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
-- |
--
-- N.B. Module extracted primarily for testability.
--
module Restyler.Run
    ( callRestylers
    ) where

import ClassyPrelude

import Restyler.Config
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Process (callProcess)

callRestylers :: Config -> [FilePath] -> IO ()
callRestylers Config{..} paths' = do
    paths <- filterM doesFileExist paths'
    traverse_ (callRestyler paths) cRestylers

callRestyler :: [FilePath] -> Restyler -> IO ()
callRestyler paths r = do
    cwd <- getCurrentDirectory
    callProcess "docker" $ dockerArguments cwd r paths

dockerArguments :: FilePath -> Restyler -> [FilePath] -> [String]
dockerArguments dir r@Restyler{..} paths =
    [ "run", "--rm"
    , "--volume", dir <> ":/code"
    , "--net", "none"
    , "restyled/restyler-" <> rName
    , rCommand
    ]
    ++ rArguments
    ++ restylePaths r paths
