module Restyler.GHA.Output
  ( GitHubOutput (..)
  , envGitHubOutput
  , HasGitHubOutput (..)
  , appendGitHubOutput
  ) where

import Restyler.Prelude

import Env qualified

newtype GitHubOutput = GitHubOutput
  { unwrap :: FilePath
  }
  deriving newtype (IsString)

class HasGitHubOutput env where
  githubOutputL :: Lens' env GitHubOutput

envGitHubOutput :: Env.Parser Env.Error GitHubOutput
envGitHubOutput = Env.var Env.nonempty "GITHUB_OUTPUT" mempty

appendGitHubOutput
  :: (MonadIO m, MonadReader env m, HasGitHubOutput env)
  => Text
  -> m ()
appendGitHubOutput x = do
  path <- view githubOutputL
  liftIO $ appendFileText path.unwrap x
