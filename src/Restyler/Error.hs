{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Restyler.Error
  ( Error (..)
  , runErrorHandlers
  , tryErrorHandlers
  , errorHandlers
  ) where

import Restyler.Prelude hiding (error)

import Restyler.AnnotatedException
import Restyler.Clone (CloneTimeoutError)
import Restyler.Config (ConfigError (..))
import Restyler.GitHub.Api (GitHubError)
import Restyler.Job.PlanUpgradeRequired (PlanUpgradeRequired)
import Restyler.Job.RepoDisabled (RepoDisabled)
import Restyler.Restyler.Run
  ( RestylerCommandNotFound
  , RestylerExitFailure
  , RestylerOutOfMemory
  , RestylerPullFailure
  , TooManyChangedPaths
  )

-- | Try our error handlers, returning /unknown/ if none match
runErrorHandlers :: Applicative m => AnnotatedException SomeException -> m Error
runErrorHandlers = fmap (fromMaybe unknown) . tryErrorHandlers

-- | Try our error handlers, returning 'Nothing' if none match
tryErrorHandlers
  :: Applicative m => AnnotatedException SomeException -> m (Maybe Error)
tryErrorHandlers aex = go errorHandlers
 where
  go [] = pure Nothing
  go (Handler f : hs)
    | Just ex <- fromException (toException aex) = Just <$> f ex
    | otherwise = go hs

errorHandlers :: Applicative m => [Handler m Error]
errorHandlers =
  [ errorHandler @RepoDisabled
      $ const
      $ warning
        { tag = "repo-disabled"
        , description = "repo disabled"
        , exitCode = ExitSuccess
        }
  , errorHandler @PlanUpgradeRequired
      $ const
      $ warning
        { tag = "plan-upgrade-required"
        , description = "plan upgrade required"
        , exitCode = ExitFailure 3
        }
  , errorHandler @CloneTimeoutError
      $ const
      $ error
        { tag = "clone-timeout"
        , description = "clone timed out"
        , exitCode = ExitFailure 5
        }
  , errorHandler @ConfigError $ \case
      ConfigErrorInvalidYaml {} ->
        warning
          { tag = "invalid-config"
          , description = "restyled.yaml is invalid"
          , exitCode = ExitFailure 10
          }
      ConfigErrorInvalidRestylers {} ->
        warning
          { tag = "invalid-config-restylers"
          , description = "restyled.yaml is invalid"
          , exitCode = ExitFailure 11
          }
      ConfigErrorInvalidRestylersYaml {} ->
        warning
          { tag = "invalid-restylers-yaml"
          , description = "bad Restylers manifest"
          , exitCode = ExitFailure 12
          }
  , errorHandler @RestylerExitFailure
      $ const
      $ warning
        { tag = "restyler"
        , description = "a Restyler errored"
        , exitCode = ExitFailure 20
        }
  , errorHandler @RestylerOutOfMemory
      $ const
      $ error
        { tag = "restyler-oom"
        , description = "a Restyler has used too much memory"
        , exitCode = ExitFailure 21
        }
  , errorHandler @RestylerCommandNotFound
      $ const
      $ error
        { tag = "restyler-command-not-found"
        , description = "a Restyler's command is invalid"
        , exitCode = ExitFailure 22
        }
  , errorHandler @RestylerPullFailure
      $ const
      $ warning
        { tag = "restyler-pull"
        , description = "unable to pull image"
        , exitCode = ExitFailure 23
        }
  , errorHandler @TooManyChangedPaths
      $ const
      $ warning
        { tag = "too-many-changed-paths"
        , description = "PR is too large"
        , exitCode = ExitFailure 25
        }
  , errorHandler @GitHubError
      $ const
      $ warning
        { tag = "github"
        , description = "GitHub communication error"
        , exitCode = ExitFailure 30
        }
  , errorHandler @SomeException $ const unknown
  ]

errorHandler
  :: forall ex m
   . (Applicative m, Exception ex)
  => (ex -> Error)
  -> Handler m Error
errorHandler f = Handler $ pure . f . exception

data Error = Error
  { severity :: Text
  , tag :: Text
  , description :: Text
  , exitCode :: ExitCode
  }

error :: Error
error = unknown {severity = "error"}

warning :: Error
warning = unknown {severity = "warning"}

unknown :: Error
unknown =
  Error
    { severity = "critical"
    , tag = "unknown"
    , description = "unknown error"
    , exitCode = ExitFailure 99
    }
