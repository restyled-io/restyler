module Restyler.GHA
  ( GitHubOutput (..)
  , envGitHubOutput
  , HasGitHubOutput (..)
  , setGitHubOutput
  , setGitHubOutputLn
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

setGitHubOutput
  :: (MonadIO m, MonadReader env m, HasGitHubOutput env)
  => Text
  -> ByteString
  -> m ()
setGitHubOutput name value = do
  path <- view $ githubOutputL . to (.unwrap)
  liftIO $ do
    writeFileText path $ name <> "="
    writeFileBS path value
    writeFileText path "\n"

setGitHubOutputLn
  :: (MonadIO m, MonadReader env m, HasGitHubOutput env)
  => Text
  -> ByteString
  -> m ()
setGitHubOutputLn name value = do
  path <- view $ githubOutputL . to (.unwrap)
  liftIO $ do
    writeFileText path $ name <> "<<EOM\n"
    writeFileBS path value
    writeFileText path "\nEOM\n"
