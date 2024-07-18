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
runErrorHandlers
  :: (MonadIO m, MonadLogger m)
  => SomeException
  -> m Error
runErrorHandlers e = fromMaybe (unknown e) <$> tryErrorHandlers e

-- | Try our error handlers, returning 'Nothing' if none match
tryErrorHandlers
  :: (MonadIO m, MonadLogger m)
  => SomeException
  -> m (Maybe Error)
tryErrorHandlers e = go errorHandlers
 where
  go [] = pure Nothing
  go (Handler f : hs)
    | Just ex <- fromException e = Just <$> f ex
    | otherwise = go hs

errorHandlers :: (MonadIO m, MonadLogger m) => [Handler m Error]
errorHandlers =
  [ errorHandler $ \case
      ex@RepoDisabled {} ->
        (warning ex)
          { tag = "repo-disabled"
          , description = "repo disabled"
          , exitCode = ExitSuccess
          }
  , errorHandler $ \case
      ex@PlanUpgradeRequired {} ->
        (warning ex)
          { tag = "plan-upgrade-required"
          , description = "plan upgrade required"
          , exitCode = ExitFailure 3
          }
  , errorHandler $ \case
      ex@CloneTimeoutError {} ->
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
  , errorHandler $ \case
      ex@RestylerExitFailure {} ->
        (warning ex)
          { tag = "restyler"
          , description = "a Restyler errored"
          , exitCode = ExitFailure 20
          }
  , errorHandler $ \case
      ex@RestylerOutOfMemory {} ->
        (unknown ex)
          { tag = "restyler-oom"
          , description = "a Restyler has used too much memory"
          , exitCode = ExitFailure 21
          }
  , errorHandler $ \case
      ex@RestylerCommandNotFound {} ->
        (unknown ex)
          { tag = "restyler-command-not-found"
          , description = "a Restyler's command is invalid"
          , exitCode = ExitFailure 22
          }
  , errorHandler $ \case
      ex@RestylerPullFailure {} ->
        (warning ex)
          { tag = "restyler-pull"
          , description = "unable to pull image"
          , exitCode = ExitFailure 23
          }
  , errorHandler $ \case
      ex@TooManyChangedPaths {} ->
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
  :: (MonadIO m, MonadLogger m, Exception ex)
  => (ex -> Error)
  -> Handler m Error
errorHandler f = Handler $ \aex@AnnotatedException {exception} -> do
  let md = f exception
  md <$ logDebug (displayAnnotatedException aex :# [])

data Error = Error
  { severity :: Text
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
    { severity = "error"
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
