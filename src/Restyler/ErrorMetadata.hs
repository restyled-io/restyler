{-# LANGUAGE NamedFieldPuns #-}

module Restyler.ErrorMetadata
    ( ErrorMetadata
    , errorMetadata
    , errorMetadataStatsdTags
    , errorMetadataDescription
    , errorMetadataExitCode
    ) where

import Restyler.Prelude

import Data.Aeson (ToJSON)
import Restyler.App (GitHubError(..))
import Restyler.Config (ConfigError(..))
import Restyler.Restyler.Run (RestylerError(..))

data ErrorMetadata = ErrorMetadata
    { severity :: Text
    , tag :: Text
    , description :: Text
    , exitCode :: Int
    }
    deriving stock Generic
    deriving anyclass ToJSON

errorMetadata :: SomeException -> ErrorMetadata
errorMetadata = fromMaybe unknown . getFirst . fold . handlers

errorMetadataStatsdTags :: ErrorMetadata -> [(Text, Text)]
errorMetadataStatsdTags ErrorMetadata { severity, tag } =
    [("severity", severity), ("error", tag)]

errorMetadataDescription :: ErrorMetadata -> Text
errorMetadataDescription ErrorMetadata { description } = description

errorMetadataExitCode :: ErrorMetadata -> ExitCode
errorMetadataExitCode ErrorMetadata { exitCode } = case exitCode of
    0 -> ExitSuccess
    n -> ExitFailure n

handlers :: SomeException -> [First ErrorMetadata]
handlers e =
    [ fromException e & First <&> \case
        ConfigErrorInvalidYaml{} -> ErrorMetadata
            { severity = "warning"
            , tag = "invalid-config"
            , description = "restyled.yaml is invalid"
            , exitCode = 10
            }
        ConfigErrorInvalidRestylers{} -> ErrorMetadata
            { severity = "warning"
            , tag = "invalid-config-restylers"
            , description = "restyled.yaml is invalid"
            , exitCode = 11
            }
        ConfigErrorInvalidRestylersYaml{} -> ErrorMetadata
            { severity = "error"
            , tag = "invalid-restylers-yaml"
            , description = "bad Restylers manifest"
            , exitCode = 12
            }
    , fromException e & First <&> \case
        RestyleError{} -> ErrorMetadata
            { severity = "warning"
            , tag = "restyle-error"
            , description = "Restyle failed"
            , exitCode = 25
            }
        RestylerExitFailure{} -> ErrorMetadata
            { severity = "warning"
            , tag = "restyler"
            , description = "a Restyler errored"
            , exitCode = 20
            }
    , fromException e & First <&> \case
        GitHubError{} -> ErrorMetadata
            { severity = "warning"
            , tag = "github"
            , description = "GitHub communication error"
            , exitCode = 30
            }
    ]

unknown :: ErrorMetadata
unknown = ErrorMetadata
    { severity = "critical"
    , tag = "unknown"
    , description = "internal error"
    , exitCode = 99
    }
