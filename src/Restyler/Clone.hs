{-# LANGUAGE OverloadedStrings #-}
module Restyler.Clone
    ( withinClonedRepo
    , checkoutBranch
    , changedPaths
    , commitAll
    , fetchOrigin
    , pushOrigin
    , forcePushOrigin
    , branchHeadMessage
    ) where

import Restyler.Prelude

import qualified Data.Text as T
import Restyler.App
import UnliftIO hiding (handleIO)
import UnliftIO.Directory (withCurrentDirectory)
import UnliftIO.Process (callProcess, readProcess)
import UnliftIO.Temporary (withSystemTempDirectory)

withinClonedRepo :: (MonadIO m, MonadUnliftIO m) => Text -> m a -> m a
withinClonedRepo url act = withSystemTempDirectory "" $ \dir -> do
    callProcess "git" ["clone", T.unpack url, dir]
    withCurrentDirectory dir act

checkoutBranch :: MonadIO m => Bool -> Text -> m ()
checkoutBranch b branch =
    callProcess "git"
        $ ["checkout", "--quiet"]
        ++ [ "-b" | b ]
        ++ [T.unpack branch]

changedPaths :: MonadIO m => Text -> m [FilePath]
changedPaths branch =
    lines <$> readProcess "git" ["diff", "--name-only", T.unpack branch] ""

commitAll :: MonadIO m => Text -> m ()
commitAll msg = callProcess "git" ["commit", "-am", T.unpack msg]

fetchOrigin :: MonadIO m => Text -> Text -> m Text
fetchOrigin remoteRef localRef = do
    callProcess
        "git"
        ["fetch", "origin", T.unpack $ remoteRef <> ":" <> localRef]
    pure localRef

pushOrigin :: MonadIO m => Text -> m ()
pushOrigin branch = callProcess "git" ["push", "origin", T.unpack branch]

forcePushOrigin :: MonadIO m => Text -> m ()
forcePushOrigin branch =
    callProcess "git" ["push", "--force-with-lease", "origin", T.unpack branch]

branchHeadMessage :: (MonadCatch m, MonadIO m) => Text -> m (Maybe Text)
branchHeadMessage branch =
    handleIO (const $ pure Nothing) $ Just . T.strip . T.pack <$> readProcess
        "git"
        ["log", "-n", "1", "--format=%B", T.unpack branch]
        ""
