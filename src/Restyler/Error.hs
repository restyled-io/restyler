{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Restyler.Error
  ( Error (..)
  , runErrorHandlers
  , tryErrorHandlers
  , errorHandlers
  ) where

import Restyler.Prelude

import Control.Monad.Logger.Aeson (SeriesElem)
import Data.Aeson (Value (..), decodeStrict)
import Data.Aeson.KeyMap qualified as KeyMap
import GitHub qualified
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Simple (getResponseStatus)
import Network.HTTP.Types.Status (statusCode)
import Restyler.AnnotatedException
import Restyler.Clone (CloneTimeoutError (..))
import Restyler.Config (ConfigError (..))
import Restyler.Job.PlanUpgradeRequired (PlanUpgradeRequired (..))
import Restyler.Job.RepoDisabled (RepoDisabled (..))
import Restyler.Restyler.Run
  ( RestylerCommandNotFound (..)
  , RestylerExitFailure (..)
  , RestylerOutOfMemory (..)
  , RestylerPullFailure (..)
  , TooManyChangedPaths (..)
  )

-- | Try our error handlers, returning /unknown/ if none match
runErrorHandlers :: Applicative m => AnnotatedException SomeException -> m Error
runErrorHandlers aex = fromMaybe (unknown e) <$> tryErrorHandlers aex
 where
  e = toException aex

-- | Try our error handlers, returning 'Nothing' if none match
tryErrorHandlers
  :: Applicative m => AnnotatedException SomeException -> m (Maybe Error)
tryErrorHandlers aex = go errorHandlers
 where
  e = toException aex
  go [] = pure Nothing
  go (Handler f : hs)
    | Just ex <- fromException e = Just <$> f ex
    | otherwise = go hs

errorHandlers :: Applicative m => [Handler m Error]
errorHandlers =
  [ errorHandler $ \ex@RepoDisabled {} ->
      (warning ex)
        { tag = "repo-disabled"
        , description = "repo disabled"
        , exitCode = ExitSuccess
        }
  , errorHandler $ \ex@PlanUpgradeRequired {} ->
      (warning ex)
        { tag = "plan-upgrade-required"
        , description = "plan upgrade required"
        , exitCode = ExitFailure 3
        }
  , errorHandler $ \ex@CloneTimeoutError {} ->
      (unknown ex)
        { tag = "clone-timeout"
        , description = "clone timed out"
        , exitCode = ExitFailure 5
        }
  , errorHandler $ \case
      ex@ConfigErrorInvalidYaml {} ->
        (warning ex)
          { tag = "invalid-config"
          , description = "restyled.yaml is invalid"
          , exitCode = ExitFailure 10
          }
      ex@ConfigErrorInvalidRestylers {} ->
        (warning ex)
          { tag = "invalid-config-restylers"
          , description = "restyled.yaml is invalid"
          , exitCode = ExitFailure 11
          }
      ex@ConfigErrorInvalidRestylersYaml {} ->
        (unknown ex)
          { tag = "invalid-restylers-yaml"
          , description = "bad Restylers manifest"
          , exitCode = ExitFailure 12
          }
  , errorHandler $ \ex@RestylerExitFailure {} ->
      (warning ex)
        { tag = "restyler"
        , description = "a Restyler errored"
        , exitCode = ExitFailure 20
        }
  , errorHandler $ \ex@RestylerOutOfMemory {} ->
      (unknown ex)
        { tag = "restyler-oom"
        , description = "a Restyler has used too much memory"
        , exitCode = ExitFailure 21
        }
  , errorHandler $ \ex@RestylerCommandNotFound {} ->
      (unknown ex)
        { tag = "restyler-command-not-found"
        , description = "a Restyler's command is invalid"
        , exitCode = ExitFailure 22
        }
  , errorHandler $ \ex@RestylerPullFailure {} ->
      (warning ex)
        { tag = "restyler-pull"
        , description = "unable to pull image"
        , exitCode = ExitFailure 23
        }
  , errorHandler $ \ex@TooManyChangedPaths {} ->
      (warning ex)
        { tag = "too-many-changed-paths"
        , description = "PR is too large"
        , exitCode = ExitFailure 25
        }
  , errorHandler $ \case
      ex@(GitHub.HTTPError (HttpExceptionRequest req (StatusCodeException resp body))) ->
        (github ex)
          { message =
              ( "GitHub request for "
                  <> decodeUtf8 @Text (HTTP.path req)
                  <> " responded "
                  <> show @Text (statusCode $ getResponseStatus resp)
              )
                :# bodyToSeries body
          }
      ex -> github ex
  , errorHandler $ unknown @SomeException
  ]

errorHandler
  :: (Applicative m, Exception ex) => (ex -> Error) -> Handler m Error
errorHandler f = Handler $ \aex@AnnotatedException {exception} ->
  pure $ setException (hide aex) $ f exception
 where
  setException :: AnnotatedException SomeException -> Error -> Error
  setException aex err = err {exception = Just aex}

data Error = Error
  { exception :: Maybe (AnnotatedException SomeException)
  , severity :: Text
  , tag :: Text
  , description :: Text
  , message :: Message
  , exitCode :: ExitCode
  }

github :: Exception ex => ex -> Error
github ex =
  (warning ex)
    { tag = "github"
    , description = "GitHub communication error"
    , exitCode = ExitFailure 30
    }

warning :: Exception ex => ex -> Error
warning ex = (unknown ex) {severity = "warning"}

unknown :: Exception ex => ex -> Error
unknown ex =
  Error
    { exception = Nothing
    , severity = "error"
    , tag = "unknown"
    , description = "unknown error"
    , message = pack (displayException ex) :# []
    , exitCode = ExitFailure 99
    }

bodyToSeries :: ByteString -> [SeriesElem]
bodyToSeries body =
  fromMaybe
    ["body" .= decodeUtf8 @Text body]
    $ valueToSeries
    =<< decodeStrict body

valueToSeries :: Value -> Maybe [SeriesElem]
valueToSeries = \case
  Object km -> Just $ map (uncurry (.=)) $ KeyMap.toList km
  _ -> Nothing
