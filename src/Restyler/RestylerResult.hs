-- |
--
-- Module      : Restyler.RestylerResult
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.RestylerResult
  ( RestylerResult (..)
  , getRestylerResult
  ) where

import Restyler.Prelude

import Restyler.Config
import Restyler.Config.CommitTemplate
import Restyler.Monad.Git
import Restyler.Options.NoCommit
import Restyler.Restyler

data RestylerResult = RestylerResult
  { restyler :: Restyler
  , restyled :: NonEmpty FilePath
  , sha :: Maybe String
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- | Build a @'RestylerResult'@ based on what @git@ says
--
-- N.B. This will create commits if appropriate.
getRestylerResult
  :: ( MonadGit m
     , MonadReader env m
     , HasNoCommit env
     )
  => Config
  -> [FilePath]
  -> Restyler
  -> m (Maybe RestylerResult)
getRestylerResult config paths restyler = do
  noCommit <- asks getNoCommit
  mRestyled <- nonEmpty . filter (`elem` paths) <$> gitDiffNameOnly Nothing

  for mRestyled $ \restyled -> do
    sha <-
      if noCommit
        then pure Nothing
        else Just <$> gitCommit commitMessage restyled

    pure
      $ RestylerResult
        { restyler
        , restyled
        , sha
        }
 where
  inputs = CommitTemplateInputs {restyler}
  commitMessage = renderCommitTemplate inputs $ cCommitTemplate config
