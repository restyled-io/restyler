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
  liftIO $ writeFileBS path $ encodeUtf8 name <> "=" <> value <> "\n"

setGitHubOutputLn
  :: (MonadIO m, MonadReader env m, HasGitHubOutput env)
  => Text
  -> ByteString
  -> m ()
setGitHubOutputLn name value = do
  path <- view $ githubOutputL . to (.unwrap)
  liftIO
    $ writeFileBS path
    $ mconcat
      [ encodeUtf8 name <> "<<EOM\n"
      , value <> "\n"
      , "EOM\n"
      ]
