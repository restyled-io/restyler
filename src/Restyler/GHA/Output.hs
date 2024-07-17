module Restyler.GHA.Output
  ( GitHubOutput
  , envGitHubOutput
  , HasGitHubOutput (..)
  , appendGitHubOutput

    -- * @DerivingVia@
  , NullGitHubOutput (..)
  ) where

import Restyler.Prelude

import Env qualified

data GitHubOutput = GitHubOutputNull | GitHubOutput FilePath

instance IsString GitHubOutput where
  fromString = GitHubOutput

class HasGitHubOutput a where
  getGitHubOutput :: a -> GitHubOutput

envGitHubOutput :: Env.Parser Env.Error GitHubOutput
envGitHubOutput = Env.var Env.nonempty "GITHUB_OUTPUT" mempty

appendGitHubOutput
  :: (MonadIO m, MonadReader env m, HasGitHubOutput env)
  => Text
  -> m ()
appendGitHubOutput x = do
  gho <- asks getGitHubOutput
  case gho of
    GitHubOutputNull -> pure ()
    GitHubOutput path -> liftIO $ appendFileText path x

newtype NullGitHubOutput a = NullGitHubOutput a

instance HasGitHubOutput (NullGitHubOutput a) where
  getGitHubOutput = const GitHubOutputNull
