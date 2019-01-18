{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Model.Restyler.Run
    ( RestyleOutcome(..)
    , RestylerResult(..)
    , restylerCommittedChanges
    , runRestylers
    ) where

import Restyler.Prelude

import Data.List (nub)
import Restyler.App
import Restyler.Config.Include
import Restyler.Config.Interpreter
import qualified Restyler.Content as Content
import Restyler.Model.Restyler

data RestyleOutcome
    = NoPaths
    | NoChanges
    | ChangesCommitted [FilePath] Text
    deriving Show

data RestylerResult = RestylerResult
    { rrRestyler :: Restyler
    , rrOutcome :: RestyleOutcome
    }

instance Show RestylerResult where
    show RestylerResult {..} = rName rrRestyler <> ": " <> show rrOutcome

restylerCommittedChanges :: RestylerResult -> Bool
restylerCommittedChanges = committedChanges . rrOutcome
  where
    committedChanges (ChangesCommitted _ _) = True
    committedChanges _ = False

-- | Runs the given @'Restyler'@s over the files
runRestylers :: MonadApp m => [Restyler] -> [FilePath] -> m [RestylerResult]
runRestylers restylers allPaths = do
    paths <- filterM doesFileExist allPaths

    logDebugN $ "Restylers: " <> tshow (map rName restylers)
    logDebugN $ "Paths: " <> tshow paths

    for restylers $ \r -> runRestyler r =<< filterRestylePaths r paths

runRestyler :: MonadApp m => Restyler -> [FilePath] -> m RestylerResult
runRestyler r [] = pure $ RestylerResult r NoPaths
runRestyler r@Restyler {..} paths = do
    if rSupportsMultiplePaths
        then do
            logInfoN $ "Restyling " <> tshow paths <> " via " <> pack rName
            dockerRunRestyler r paths
        else for_ paths $ \path -> do
            logInfoN $ "Restyling " <> tshow path <> " via " <> pack rName
            dockerRunRestyler r [path]

    RestylerResult r <$> getRestyleOutcome r

getRestyleOutcome :: MonadApp m => Restyler -> m RestyleOutcome
getRestyleOutcome restyler = do
    changedPaths <- gitDiffNameOnly

    if null changedPaths
        then pure NoChanges
        else ChangesCommitted changedPaths <$> commitChanges
  where
    gitDiffNameOnly = lines <$> readProcess "git" ["diff", "--name-only"] ""
    gitCommitAll msg = callProcess "git" ["commit", "-a", "--message", msg]
    gitRevParseHead = readProcess "git" ["rev-parse", "HEAD"] ""

    commitChanges = do
        gitCommitAll $ unpack $ Content.commitMessage restyler
        chomp . pack <$> gitRevParseHead

filterRestylePaths :: MonadApp m => Restyler -> [FilePath] -> m [FilePath]
filterRestylePaths r = filterM (r `shouldRestyle`)
  where
    Restyler {..} `shouldRestyle` path
        | includePath rInclude path = pure True
        | null rInterpreters = pure False
        | otherwise = do
            contents <- readFile path
            pure $ any (contents `hasInterpreter`) rInterpreters

dockerRunRestyler :: MonadApp m => Restyler -> [FilePath] -> m ()
dockerRunRestyler Restyler {..} paths = do
    cwd <- getCurrentDirectory
    callProcess "docker"
        $ ["run", "--rm", "--net", "none", "--volume", cwd <> ":/code", rImage]
        <> nub (rCommand <> rArguments)
        <> [ "--" | rSupportsArgSep ]
        <> map ("./" <>) paths
