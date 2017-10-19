{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Restyler.Clone
    ( withinClonedRepo
    , checkoutBranch
    , changedPaths
    , commitAll
    , pushOrigin
    ) where

import ClassyPrelude

import System.Directory (withCurrentDirectory)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess, readProcess)

withinClonedRepo :: Text -> IO a -> IO a
withinClonedRepo url act = withSystemTempDirectory "" $ \dir -> do
    callProcess "git" ["clone", unpack url, dir]
    withCurrentDirectory dir act

checkoutBranch :: Bool -> Text -> IO ()
checkoutBranch b branch = callProcess "git" $
    ["checkout", "--quiet"] ++ ["-b" | b] ++ [unpack branch]

changedPaths :: Text -> IO [FilePath]
changedPaths branch = lines <$>
    readProcess "git" ["diff", "--name-only", unpack branch] ""

commitAll :: Text -> IO ()
commitAll msg = callProcess "git" ["commit", "-am", unpack msg]

pushOrigin :: Text -> IO ()
pushOrigin branch = callProcess "git" ["push", "origin", unpack branch]
