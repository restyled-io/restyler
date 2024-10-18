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

import Data.Aeson (ToJSON)
import Restyler.Config
import Restyler.Monad.Git
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
     , HasCommitTemplate env
     , HasNoCommit env
     )
  => [FilePath]
  -> Restyler
  -> m (Maybe RestylerResult)
getRestylerResult paths restyler = do
  template <- asks getCommitTemplate
  noCommit <- asks getNoCommit
  mRestyled <- nonEmpty . filter (`elem` paths) <$> gitDiffNameOnly Nothing

  for mRestyled $ \restyled -> do
    sha <-
      if noCommit
        then pure Nothing
        else do
          ref <- gitCommit (renderCommitTemplate inputs template) restyled
          pure $ Just ref

    pure
      $ RestylerResult
        { restyler
        , restyled
        , sha
        }
 where
  inputs = CommitTemplateInputs {restyler}
