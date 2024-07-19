module Restyler.GHA.Output
  ( GitHubOutput
  , envGitHubOutput
  , HasGitHubOutput (..)
  , appendGitHubOutputs
  , appendGitHubOutput

    -- * @DerivingVia@
  , NullGitHubOutput (..)
  ) where

import Restyler.Prelude

import Data.Text qualified as T
import Env qualified

data GitHubOutput = GitHubOutputNull | GitHubOutput FilePath

instance IsString GitHubOutput where
  fromString = GitHubOutput

class HasGitHubOutput a where
  getGitHubOutput :: a -> GitHubOutput

envGitHubOutput :: Env.Parser Env.Error GitHubOutput
envGitHubOutput = Env.var Env.nonempty "GITHUB_OUTPUT" mempty

appendGitHubOutputs
  :: (MonadIO m, MonadReader env m, HasGitHubOutput env)
  => [Text]
  -> m ()
appendGitHubOutputs = traverse_ appendGitHubOutput

appendGitHubOutput
  :: (MonadIO m, MonadReader env m, HasGitHubOutput env)
  => Text
  -> m ()
appendGitHubOutput x = do
  gho <- asks getGitHubOutput
  case gho of
    GitHubOutputNull -> pure ()
    GitHubOutput path -> do
      liftIO $ appendFileText path $ ensureNewline x

ensureNewline :: Text -> Text
ensureNewline t
  | Just (_, '\n') <- T.unsnoc t = t
  | otherwise = T.snoc t '\n'

newtype NullGitHubOutput a = NullGitHubOutput a

instance HasGitHubOutput (NullGitHubOutput a) where
  getGitHubOutput = const GitHubOutputNull
