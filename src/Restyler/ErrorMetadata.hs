module Restyler.ErrorMetadata
  ( withExitHandler
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
  , TooManyChangedPaths (..)
  )

withExitHandler :: (MonadUnliftIO m, MonadLogger m) => m a -> m ()
withExitHandler f = do
  ec <- (ExitSuccess <$ f) `catches` handlers
  exitWith ec

handlers :: (MonadIO m, MonadLogger m) => [Handler m ExitCode]
handlers =
  [ toHandler $ \case
      ex@RepoDisabled {} ->
        (warning ex)
          { tag = "repo-disabled"
          , description = "repo disabled"
          , exitCode = ExitSuccess
          }
  , toHandler $ \case
      ex@PlanUpgradeRequired {} ->
        (warning ex)
          { tag = "plan-upgrade-required"
          , description = "plan upgrade required"
          , exitCode = ExitFailure 3
          }
  , toHandler $ \case
      ex@CloneTimeoutError {} ->
        (unknown ex)
          { tag = "clone-timeout"
          , description = "clone timed out"
          , exitCode = ExitFailure 5
          }
  , toHandler $ \case
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
  , toHandler $ \case
      ex@RestylerExitFailure {} ->
        (warning ex)
          { tag = "restyler"
          , description = "a Restyler errored"
          , exitCode = ExitFailure 20
          }
  , toHandler $ \case
      ex@RestylerOutOfMemory {} ->
        (unknown ex)
          { tag = "restyler-oom"
          , description = "a Restyler has used too much memory"
          , exitCode = ExitFailure 21
          }
  , toHandler $ \case
      ex@RestylerCommandNotFound {} ->
        (unknown ex)
          { tag = "restyler-command-not-found"
          , description = "a Restyler's command is invalid"
          , exitCode = ExitFailure 22
          }
  , toHandler $ \case
      ex@TooManyChangedPaths {} ->
        (warning ex)
          { tag = "too-many-changed-paths"
          , description = "PR is too large"
          , exitCode = ExitFailure 25
          }
  , toHandler $ \case
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
  , toHandler $ unknown @SomeException
  ]

github :: Exception ex => ex -> ErrorMetadata
github ex =
  (warning ex)
    { tag = "github"
    , description = "GitHub communication error"
    , exitCode = ExitFailure 30
    }

warning :: Exception ex => ex -> ErrorMetadata
warning ex = (unknown ex) {severity = "warning"}

unknown :: Exception ex => ex -> ErrorMetadata
unknown ex =
  ErrorMetadata
    { severity = "error"
    , tag = "unknown"
    , description = "unknown error"
    , message = pack (displayException ex) :# []
    , exitCode = ExitFailure 99
    }

toHandler
  :: (MonadIO m, MonadLogger m, Exception e)
  => (e -> ErrorMetadata)
  -> Handler m ExitCode
toHandler f = Handler $ \aex@AnnotatedException {exception} -> do
  let md = f exception
  logDebug $ displayAnnotatedException aex :# []
  case md.severity of
    "warning" -> logWarn md.message
    _ -> logError md.message
  pure md.exitCode

data ErrorMetadata = ErrorMetadata
  { severity :: Text
  , tag :: Text
  , description :: Text
  , message :: Message
  , exitCode :: ExitCode
  }
  deriving stock (Generic)

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
