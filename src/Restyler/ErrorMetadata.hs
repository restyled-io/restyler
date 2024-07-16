{-# LANGUAGE NamedFieldPuns #-}

module Restyler.ErrorMetadata
  ( ErrorMetadata
  , errorMetadata
  , errorMetadataStatsdTags
  , errorMetadataDescription
  , errorMetadataMessage
  , errorMetadataExitCode
  , logErrorMetadata
  , logErrorMetadataAndExit
  ) where

import Restyler.Prelude

import Control.Monad.Logger.Aeson (SeriesElem)
import Data.Aeson (Value (..), decodeStrict, object)
import Data.Aeson.KeyMap qualified as KeyMap
import GitHub qualified
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Simple (getResponseStatus)
import Network.HTTP.Types.Status (statusCode)
import Restyler.App (GitHubError (..))
import Restyler.Config (ConfigError (..))
import Restyler.Restyler.Run
  ( RestylerCommandNotFound (..)
  , RestylerExitFailure (..)
  , RestylerOutOfMemory (..)
  , TooManyChangedPaths (..)
  )
import Restyler.Setup (CloneTimeoutError (..), PlanUpgradeRequired (..))

data ErrorMetadata = ErrorMetadata
  { exception :: SomeException
  , severity :: Text
  , tag :: Text
  , description :: Text
  , message :: Maybe Message
  , exitCode :: Int
  }
  deriving stock (Generic)

errorMetadata :: SomeException -> ErrorMetadata
errorMetadata ex = fromMaybe (unknown ex) $ getFirst $ fold $ handlers ex

errorMetadataStatsdTags :: ErrorMetadata -> [(Text, Text)]
errorMetadataStatsdTags ErrorMetadata {severity, tag} =
  [("severity", severity), ("error", tag)]

errorMetadataDescription :: ErrorMetadata -> Text
errorMetadataDescription ErrorMetadata {description} = description

errorMetadataMessage :: ErrorMetadata -> Message
errorMetadataMessage ErrorMetadata {description, message} =
  fromMaybe (description :# []) message

errorMetadataExitCode :: ErrorMetadata -> ExitCode
errorMetadataExitCode ErrorMetadata {exitCode} = case exitCode of
  0 -> ExitSuccess
  n -> ExitFailure n

handlers :: SomeException -> [First ErrorMetadata]
handlers e =
  [ fromException e & First <&> \case
      PlanUpgradeRequired {} ->
        ErrorMetadata
          { exception = e
          , severity = "warning"
          , tag = "plan-upgrade-required"
          , description = "plan upgrade required"
          , message = Nothing
          , exitCode = 3
          }
  , fromException e & First <&> \case
      CloneTimeoutError {} ->
        ErrorMetadata
          { exception = e
          , severity = "error"
          , tag = "clone-timeout"
          , description = "clone timed out"
          , message = Nothing
          , exitCode = 5
          }
  , fromException e & First <&> \case
      ConfigErrorInvalidYaml {} ->
        ErrorMetadata
          { exception = e
          , severity = "warning"
          , tag = "invalid-config"
          , description = "restyled.yaml is invalid"
          , message = Nothing
          , exitCode = 10
          }
      ConfigErrorInvalidRestylers {} ->
        ErrorMetadata
          { exception = e
          , severity = "warning"
          , tag = "invalid-config-restylers"
          , description = "restyled.yaml is invalid"
          , message = Nothing
          , exitCode = 11
          }
      ConfigErrorInvalidRestylersYaml {} ->
        ErrorMetadata
          { exception = e
          , severity = "error"
          , tag = "invalid-restylers-yaml"
          , description = "bad Restylers manifest"
          , message = Nothing
          , exitCode = 12
          }
  , fromException e & First <&> \case
      RestylerExitFailure {} ->
        ErrorMetadata
          { exception = e
          , severity = "warning"
          , tag = "restyler"
          , description = "a Restyler errored"
          , message = Nothing
          , exitCode = 20
          }
  , fromException e & First <&> \case
      RestylerOutOfMemory {} ->
        ErrorMetadata
          { exception = e
          , severity = "error"
          , tag = "restyler-oom"
          , description = "a Restyler has used too much memory"
          , message = Nothing
          , exitCode = 21
          }
  , fromException e & First <&> \case
      RestylerCommandNotFound {} ->
        ErrorMetadata
          { exception = e
          , severity = "error"
          , tag = "restyler-command-not-found"
          , description = "a Restyler's command is invalid"
          , message = Nothing
          , exitCode = 22
          }
  , fromException e & First <&> \case
      TooManyChangedPaths {} ->
        ErrorMetadata
          { exception = e
          , severity = "warning"
          , tag = "too-many-changed-paths"
          , description = "PR is too large"
          , message = Nothing
          , exitCode = 25
          }
  , fromException e & First <&> \case
      GitHubError {} ->
        ErrorMetadata
          { exception = e
          , severity = "warning"
          , tag = "github"
          , description = "GitHub communication error"
          , message = Nothing
          , exitCode = 30
          }
  , fromException e & First <&> \case
      GitHub.HTTPError (HttpExceptionRequest req (StatusCodeException resp body)) ->
        ErrorMetadata
          { exception = e
          , severity = "warning"
          , tag = "github"
          , description = "GitHub communication error"
          , message =
              Just
                $ ( "GitHub request for "
                      <> decodeUtf8 @Text (HTTP.path req)
                      <> " responded "
                      <> show @Text (statusCode $ getResponseStatus resp)
                  )
                :# bodyToSeries body
          , exitCode = 30
          }
      _ ->
        ErrorMetadata
          { exception = e
          , severity = "warning"
          , tag = "github"
          , description = "GitHub communication error"
          , message = Nothing
          , exitCode = 30
          }
  ]

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

unknown :: SomeException -> ErrorMetadata
unknown e =
  ErrorMetadata
    { exception = e
    , severity = "critical"
    , tag = "unknown"
    , description = "internal error"
    , message = Nothing
    , exitCode = 99
    }

logErrorMetadata :: MonadLogger m => ErrorMetadata -> m ()
logErrorMetadata ErrorMetadata {exception, severity, tag, description, message} = do
  case message of
    -- Legacy error handling where we're expecting to log the full exception
    -- with some of the metadata fields
    Nothing ->
      logError
        $ ("Exception:\n" <> pack (displayException exception))
        :# [ "error"
              .= object
                [ "severity" .= severity
                , "tag" .= tag
                , "description" .= description
                ]
           ]
    -- Modern handling where all loggable content is expected in our message
    -- field, and we include the full exception in DEBUG in case we screw up
    Just lm -> do
      logDebug $ ("Exception:\n" <> pack (displayException exception)) :# []
      logError lm

logErrorMetadataAndExit :: (MonadIO m, MonadLogger m) => ErrorMetadata -> m a
logErrorMetadataAndExit md = do
  logErrorMetadata md
  exitWith $ errorMetadataExitCode md
