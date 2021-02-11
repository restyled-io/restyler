{-# LANGUAGE DerivingVia #-}

module Restyler.Capabilities.Git
    ( MonadGit(..)
    , ActualGit(..)
    )
where

import Restyler.Prelude

import Restyler.Capabilities.Process

class Monad m => MonadGit m where
    gitCheckout :: String -> m ()
    gitCheckoutExisting :: String -> m ()
    gitClone :: String -> FilePath -> m ()
    gitCommitAll :: String -> m String
    gitDiffNameOnly :: Maybe String -> m [FilePath]
    gitFetch :: String -> String -> m ()
    gitMerge :: String -> m ()
    gitMergeBase :: String -> m (Maybe String)
    gitPush :: String -> m ()
    gitPushForce :: String -> m ()

instance MonadGit m => MonadGit (ExceptT e m) where
    gitCheckout = lift . gitCheckout
    gitCheckoutExisting = lift . gitCheckoutExisting
    gitClone x = lift . gitClone x
    gitCommitAll = lift . gitCommitAll
    gitDiffNameOnly = lift . gitDiffNameOnly
    gitFetch x = lift . gitFetch x
    gitMerge = lift . gitMerge
    gitMergeBase = lift . gitMergeBase
    gitPush = lift . gitPush
    gitPushForce = lift . gitPushForce

newtype ActualGit m a = ActualGit
    { unActualGit :: m a
    }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader env)
    deriving MonadProcess via (ActualProcess (ActualGit m))

instance MonadUnliftIO m => MonadUnliftIO (ActualGit m) where
    {-# INLINE withRunInIO #-}
    withRunInIO inner =
        ActualGit $ withRunInIO $ \run -> inner (run . unActualGit)

instance MonadUnliftIO m => MonadGit (ActualGit m) where
    gitCheckout branch =
        callProcess "git" ["checkout", "--no-progress", "-b", branch]

    gitCheckoutExisting branch =
        callProcess "git" ["checkout", "--no-progress", branch]

    gitClone url dir = callProcess "git" ["clone", "--quiet", url, dir]

    gitCommitAll msg = do
        callProcess "git" ["commit", "-a", "--message", msg]
        dropWhileEnd isSpace <$> readProcess "git" ["rev-parse", "HEAD"] ""

    gitDiffNameOnly mRef = do
        let args = ["diff", "--name-only"] <> maybeToList mRef
        lines <$> readProcess "git" args ""

    gitFetch remoteRef localRef =
        callProcess "git" ["fetch", "origin", remoteRef <> ":" <> localRef]

    gitMerge branch = callProcess "git" ["merge", "--ff-only", branch]

    gitMergeBase branch = do
        output <- readProcess "git" ["merge-base", branch, "HEAD"] ""
        pure $ listToMaybe $ lines output

    gitPush branch = callProcess "git" ["push", "origin", branch]

    gitPushForce branch =
        callProcess "git" ["push", "--force-with-lease", "origin", branch]
