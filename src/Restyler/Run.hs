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
import Restyler.Clone
import Restyler.Config
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Process (callProcess)

callRestylers :: Text -> IO (Either String ())
callRestylers mergeBase = runExceptT $ do
    Config{..} <- ExceptT loadConfig

    unless cEnabled $
        throwError "Restyler disabled by config"

    dir <- tryE getCurrentDirectory
    paths <- tryE $ filterM doesFileExist =<< changedPaths mergeBase
    tryE $ for_ cRestylers $ \r@Restyler{..} ->
        callProcess "docker" $ dockerArguments dir r
            ++ rArguments
            ++ restylePaths r paths

dockerArguments :: FilePath -> Restyler -> [String]
dockerArguments dir Restyler{..} =
    [ "run", "--rm"
    , "--volume", dir <> ":/code"
    , "--net", "none"
    , "restyled/restyler-" <> rName
    , rCommand
    ]

tryE :: IO a -> ExceptT String IO a
tryE f = ExceptT $ first show <$> tryIO f
