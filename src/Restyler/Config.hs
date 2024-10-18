{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_HADDOCK prune, ignore-exports #-}

-- | Handling of @.restyled.yaml@ content and behavior driven there-by
--
-- __Implementation note__: This is a playground. I'm doing lots of HKD stuff
-- here that I would not normally subject my collaborators to.
--
-- 1. We only do this stuff here, and
-- 2. It should stay encapsulated away from the rest of the system
--
-- References:
--
-- - <https://reasonablypolymorphic.com/blog/higher-kinded-data/>
-- - <https://chrispenner.ca/posts/hkd-options>
-- - <https://hackage.haskell.org/package/barbies>
--
-- Module      : Restyler.Config
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Config
  ( Config (..)
  , ConfigError (..)
  , loadConfig

    -- * Exported as we move logic to "Restyler.Options"
  , formatYamlException

    -- * Exported for use in tests
  , ConfigSource (..)
  , loadConfigFrom
  , resolveRestylers
  , defaultConfigContent
  , configPaths

    -- * New stuff
  , module Restyler.Config.Parse
  ) where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Casing
import Data.FileEmbed (embedFile)
import Data.Functor.Barbie
import Data.Yaml
  ( ParseException (..)
  , YamlException (..)
  , prettyPrintParseException
  )
import Data.Yaml qualified as Yaml
import Restyler.AnnotatedException
import Restyler.Config.ChangedPaths
import Restyler.Config.CommitTemplate
import Restyler.Config.Glob
import Restyler.Config.Parse
import Restyler.Config.RemoteFile
import Restyler.Config.Restyler
import Restyler.Config.SketchyList
import Restyler.Monad.Directory
import Restyler.Monad.DownloadFile
import Restyler.Monad.ReadFile
import Restyler.Options.Manifest
import Restyler.Restyler
import Restyler.Wiki qualified as Wiki
import Restyler.Yaml.Errata (formatInvalidYaml)

-- | A polymorphic representation of @'Config'@
--
-- 1. The @f@ parameter can dictate if attributes are required (@'Identity'@) or
--    optional (@'Maybe'@), or optional with override semantics (@'Last'@)
--
-- 2. Any list keys use @'SketchyList'@ so users can type a single scalar
--    element or a list of many elements.
--
-- 3. The @Restylers@ attribute is a (sketchy) list of @'ConfigRestyler'@, which
--    is a function to apply to the later-fetched list of all Restylers.
--
-- See the various @resolve@ functions for how to get a real @'Config'@ out of
-- this beast.
data ConfigF f = ConfigF
  { cfChangedPaths :: f ChangedPathsConfig
  , cfCommitTemplate :: f CommitTemplate
  , cfRemoteFiles :: f (SketchyList RemoteFile)
  , cfIgnoreAuthors :: f (SketchyList (Glob Text))
  , cfIgnoreLabels :: f (SketchyList (Glob Text))
  , cfIgnoreBranches :: f (SketchyList (Glob Text))
  , cfRestylersVersion :: f String
  , cfRestylers :: f (SketchyList RestylerOverride)
  }
  deriving stock (Generic)
  deriving anyclass (FunctorB, ApplicativeB, ConstraintsB)

-- | An empty @'ConfigF'@ of all @'Nothing'@s
--
-- N.B. the choice of @'getAlt'@ is somewhat arbitrary. We just need a @Maybe@
-- wrapper @f a@ where @getX mempty@ is @Nothing@, but without a @Monoid a@
-- constraint.
emptyConfig :: ConfigF Maybe
emptyConfig = bmap getAlt bmempty

instance FromJSON (ConfigF Maybe) where
  parseJSON a@(Array _) = do
    restylers <- parseJSON a
    pure emptyConfig {cfRestylers = restylers}
  parseJSON v = genericParseJSON (aesonPrefix snakeCase) v

instance FromJSON (ConfigF Identity) where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- | Fill out one @'ConfigF'@ from another
resolveConfig :: ConfigF Maybe -> ConfigF Identity -> ConfigF Identity
resolveConfig = bzipWith f
 where
  f :: Maybe a -> Identity a -> Identity a
  f ma ia = maybe ia Identity ma

-- | Fully resolved configuration
--
-- This is what we work with throughout the system.
data Config = Config
  { cChangedPaths :: ChangedPathsConfig
  , cCommitTemplate :: CommitTemplate
  , cRemoteFiles :: [RemoteFile]
  , cIgnoreAuthors :: [Glob Text]
  , cIgnoreBranches :: [Glob Text]
  , cIgnoreLabels :: [Glob Text]
  , cRestylers :: [Restyler]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Config where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

data ConfigError
  = ConfigErrorInvalidYaml FilePath ByteString ParseException
  | ConfigErrorInvalidRestylers [Text]
  | ConfigErrorInvalidRestylersYaml SomeException
  deriving stock (Show)

instance Exception ConfigError where
  displayException =
    unpack . unlines . \case
      ConfigErrorInvalidYaml path yaml e ->
        [ formatYamlException path yaml e
        , ""
        , generalHelp
        ]
      ConfigErrorInvalidRestylers errs -> errs <> ["", generalHelp]
      ConfigErrorInvalidRestylersYaml ex ->
        [ "Error loading restylers.yaml definition:"
        , show @Text ex
        , ""
        , "==="
        , ""
        , "This could be caused by an invalid or too-old restylers_version in"
        , "your configuration. Consider removing or updating it."
        , ""
        , "If that's not the case, this is a bug in our system that we are"
        , "hopefully already working to fix."
        , ""
        , "- " <> Wiki.page "Restyler Versions"
        , "- " <> generalHelp
        ]
   where
    generalHelp :: Text
    generalHelp = Wiki.commonError ".restyled.yaml"

formatYamlException :: FilePath -> ByteString -> ParseException -> Text
formatYamlException path bs = \case
  InvalidYaml (Just (YamlParseException problem context mark)) ->
    formatInvalidYaml path bs problem context mark
  ex ->
    unlines
      [ pack path <> " is not valid yaml"
      , "Exception:"
      , pack $ prettyPrintParseException ex
      , ""
      , "Input:"
      , decodeUtf8 bs
      ]

-- | Load a fully-inflated @'Config'@
--
-- Read any @.restyled.yaml@, fill it out from defaults, grab the versioned set
-- of restylers data, and apply the configured choices and overrides.
loadConfig
  :: ( MonadUnliftIO m
     , MonadDirectory m
     , MonadReadFile m
     , MonadDownloadFile m
     , MonadReader env m
     , HasManifest env
     )
  => m Config
loadConfig =
  loadConfigFrom (map ConfigPath configPaths)
    $ handleTo ConfigErrorInvalidRestylersYaml
    . getAllRestylersVersioned
    . runIdentity
    . cfRestylersVersion

loadConfigFrom
  :: (MonadUnliftIO m, MonadDirectory m, MonadReadFile m)
  => [ConfigSource]
  -> (ConfigF Identity -> m [Restyler])
  -> m Config
loadConfigFrom sources f = do
  config <- loadConfigF sources
  restylers <- f config
  resolveRestylers config restylers

data ConfigSource
  = ConfigPath FilePath
  | ConfigContent ByteString

readConfigSources
  :: (MonadDirectory m, MonadReadFile m)
  => [ConfigSource]
  -> m (Maybe (FilePath, ByteString))
readConfigSources = runMaybeT . asum . fmap (MaybeT . go)
 where
  go
    :: (MonadDirectory m, MonadReadFile m)
    => ConfigSource
    -> m (Maybe (FilePath, ByteString))
  go = \case
    ConfigPath path -> do
      exists <- doesFileExist path
      if exists then Just . (path,) <$> readFileBS path else pure Nothing
    ConfigContent content -> pure $ Just ("<config>", content)

-- | Load configuration if present and apply defaults
--
-- Returns @'ConfigF' 'Identity'@ because defaulting has populated all fields.
--
-- May throw any @'ConfigError'@. May through raw @'ParseException'@s if
-- there is a programmer error in our static default configuration YAML.
loadConfigF
  :: ( MonadUnliftIO m
     , MonadDirectory m
     , MonadReadFile m
     )
  => [ConfigSource]
  -> m (ConfigF Identity)
loadConfigF sources =
  resolveConfig
    <$> loadUserConfigF sources
    <*> decodeThrow defaultConfigContent

loadUserConfigF
  :: ( MonadUnliftIO m
     , MonadDirectory m
     , MonadReadFile m
     )
  => [ConfigSource]
  -> m (ConfigF Maybe)
loadUserConfigF = maybeM (pure emptyConfig) (uncurry decodeThrow') . readConfigSources

-- | @'decodeThrow'@, but wrapping YAML parse errors to @'ConfigError'@
decodeThrow' :: (MonadUnliftIO m, FromJSON a) => FilePath -> ByteString -> m a
decodeThrow' path content =
  handleTo (ConfigErrorInvalidYaml path content) $ decodeThrow content

decodeThrow :: (MonadIO m, FromJSON a) => ByteString -> m a
decodeThrow = either throw pure . Yaml.decodeThrow

-- | Populate @'cRestylers'@ using the versioned restylers data
--
-- May throw @'ConfigErrorInvalidRestylers'@.
resolveRestylers :: MonadIO m => ConfigF Identity -> [Restyler] -> m Config
resolveRestylers ConfigF {..} allRestylers = do
  restylers <-
    either (throw . ConfigErrorInvalidRestylers) pure
      $ overrideRestylers allRestylers
      $ unSketchy
      $ runIdentity cfRestylers

  pure
    Config
      { cChangedPaths = runIdentity cfChangedPaths
      , cCommitTemplate = runIdentity cfCommitTemplate
      , cRemoteFiles = unSketchy $ runIdentity cfRemoteFiles
      , cIgnoreAuthors = unSketchy $ runIdentity cfIgnoreAuthors
      , cIgnoreBranches = unSketchy $ runIdentity cfIgnoreBranches
      , cIgnoreLabels = unSketchy $ runIdentity cfIgnoreLabels
      , cRestylers = restylers
      }

defaultConfigContent :: ByteString
defaultConfigContent = $(embedFile "config/default.yaml")

configPaths :: [FilePath]
configPaths =
  [ ".restyled.yaml"
  , ".restyled.yml"
  , ".github/restyled.yaml"
  , ".github/restyled.yml"
  ]
