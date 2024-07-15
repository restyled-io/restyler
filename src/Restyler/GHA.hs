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
  :: (MonadIO m, MonadLogger m, MonadReader env m, HasGitHubOutput env)
  => Text
  -> Text
  -> m ()
setGitHubOutput name value = do
  path <- view $ githubOutputL . to (.unwrap)
  let content = name <> "=" <> value <> "\n"
  logInfo
    $ "Setting GitHub Output"
    :# [ "GITHUB_OUTPUT" .= path
       , "content" .= content
       ]

  liftIO $ writeFileText path content

setGitHubOutputLn
  :: (MonadIO m, MonadLogger m, MonadReader env m, HasGitHubOutput env)
  => Text
  -> Text
  -> m ()
setGitHubOutputLn name value = do
  path <- view $ githubOutputL . to (.unwrap)
  let content =
        mconcat
          [ name <> "<<EOM\n"
          , value <> "\n"
          , "EOM\n"
          ]
  logInfo
    $ "Setting GitHub Output"
    :# [ "GITHUB_OUTPUT" .= path
       , "content" .= content
       ]

  liftIO $ writeFileText path content
