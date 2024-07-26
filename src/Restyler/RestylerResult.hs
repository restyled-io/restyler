module Restyler.RestylerResult
  ( RestylerResult (..)
  , getRestylerResult
  ) where

import Restyler.Prelude

import Restyler.Config
import Restyler.Config.CommitTemplate
import Restyler.Git
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
  :: (MonadGit m, MonadReader env m, HasNoCommitOption env)
  => Config
  -> Restyler
  -> m (Maybe RestylerResult)
getRestylerResult config restyler = do
  noCommit <- getNoCommit
  mRestyled <- nonEmpty <$> gitDiffNameOnly Nothing

  for mRestyled $ \restyled -> do
    sha <-
      if noCommit
        then pure Nothing
        else Just <$> gitCommitAll commitMessage

    pure
      $ RestylerResult
        { restyler
        , restyled
        , sha
        }
 where
  inputs = CommitTemplateInputs {restyler}
  commitMessage = renderCommitTemplate inputs $ cCommitTemplate config
