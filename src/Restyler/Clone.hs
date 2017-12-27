{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Restyler.Clone
    ( withinClonedRepo
    , checkoutBranch
    , changedPaths
    , commitAll
    , pushOrigin
    , forcePushOrigin
    , branchHeadMessage
    ) where

import ClassyPrelude

import qualified Data.Text as T
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

forcePushOrigin :: Text -> IO ()
forcePushOrigin branch = callProcess "git"
    ["push", "--force-with-lease", "origin", unpack branch]

branchHeadMessage :: Text -> IO (Maybe Text)
branchHeadMessage branch = handle errNothing $ do
    output <- readProcess "git" ["log", "-n", "1", "--format='%B'", unpack branch] ""
    return $ Just $ T.strip $ pack output
  where
    errNothing :: Monad m => IOException -> m (Maybe a)
    errNothing _ = return Nothing
