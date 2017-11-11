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

import Control.Monad.Except hiding (filterM)
import Data.Bifunctor (first)
import Restyler.Config
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Process (callProcess)

callRestylers :: [FilePath] -> IO (Either String ())
callRestylers paths' = runExceptT $ do
    Config{..} <- ExceptT loadConfig

    unless cEnabled $
        throwError "Restyler disabled by config"

    tryE $ do
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

tryE :: IO a -> ExceptT String IO a
tryE f = ExceptT $ first show <$> tryIO f
