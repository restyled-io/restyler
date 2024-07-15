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
module Restyler.Config
  ( Config (..)
  , ConfigError (..)
  , configPullRequestReviewer
  , loadConfig
  , HasConfig (..)
  , whenConfig
  , whenConfigNonEmpty
  , whenConfigJust

    -- * Exported for use in tests
  , ConfigSource (..)
  , loadConfigFrom
  , resolveRestylers
  , defaultConfigContent
  , configPaths
  ) where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Casing
import Data.FileEmbed (embedFile)
import Data.Functor.Barbie
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Yaml
  ( ParseException (..)
  , YamlException (..)
  , prettyPrintParseException
  )
import Data.Yaml qualified as Yaml
import GitHub.Data (IssueLabel, User)
import Restyler.App.Class
import Restyler.CommitTemplate
import Restyler.Config.ChangedPaths
import Restyler.Config.ExpectedKeys
import Restyler.Config.Glob
import Restyler.Config.RequestReview
import Restyler.Config.Restyler
import Restyler.Config.SketchyList
import Restyler.Config.Statuses
import Restyler.ManifestOption
import Restyler.PullRequest
import Restyler.RemoteFile
import Restyler.Restyler
import Restyler.Wiki qualified as Wiki
import Restyler.Yaml.Errata (formatInvalidYaml)
import UnliftIO.Exception (handle)

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
  { cfEnabled :: f Bool
  , cfExclude :: f (SketchyList (Glob FilePath))
  , cfAlsoExclude :: f (SketchyList (Glob FilePath))
  , cfChangedPaths :: f ChangedPathsConfig
  , cfAuto :: f Bool
  , cfCommitTemplate :: f CommitTemplate
  , cfRemoteFiles :: f (SketchyList RemoteFile)
  , cfPullRequests :: f Bool
  , cfComments :: f Bool
  , cfStatuses :: f Statuses
  , cfRequestReview :: f RequestReviewConfig
  , cfLabels :: f (SketchyList (Name IssueLabel))
  , cfIgnoreAuthors :: f (SketchyList (Glob (Name User)))
  , cfIgnoreLabels :: f (SketchyList (Glob (Name IssueLabel)))
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
  parseJSON v = genericParseJSONValidated (aesonPrefix snakeCase) v

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
  { cEnabled :: Bool
  , cExclude :: Set (Glob FilePath)
  , cChangedPaths :: ChangedPathsConfig
  , cAuto :: Bool
  , cCommitTemplate :: CommitTemplate
  , cRemoteFiles :: [RemoteFile]
  , cPullRequests :: Bool
  , cComments :: Bool
  , cStatuses :: Statuses
  , cRequestReview :: RequestReviewConfig
  , cLabels :: Set (Name IssueLabel)
  , cIgnoreAuthors :: [Glob (Name User)]
  , cIgnoreBranches :: [Glob Text]
  , cIgnoreLabels :: [Glob (Name IssueLabel)]
  , cRestylers :: [Restyler]
  }
  deriving stock (Eq, Show, Generic)

-- | If so configured, return the @'User'@ from whom to request review
configPullRequestReviewer :: PullRequest -> Config -> Maybe (Name User)
configPullRequestReviewer pr = determineReviewer pr . cRequestReview

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
     , MonadLogger m
     , MonadSystem m
     , MonadDownloadFile m
     , MonadReader env m
     , HasManifestOption env
     )
  => m Config
loadConfig =
  loadConfigFrom (map ConfigPath configPaths)
    $ handleTo ConfigErrorInvalidRestylersYaml
    . getAllRestylersVersioned
    . runIdentity
    . cfRestylersVersion

loadConfigFrom
  :: (MonadUnliftIO m, MonadSystem m)
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
  :: MonadSystem m => [ConfigSource] -> m (Maybe (FilePath, ByteString))
readConfigSources = runMaybeT . asum . fmap (MaybeT . go)
 where
  go :: MonadSystem m => ConfigSource -> m (Maybe (FilePath, ByteString))
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
  :: (MonadUnliftIO m, MonadSystem m)
  => [ConfigSource]
  -> m (ConfigF Identity)
loadConfigF sources =
  resolveConfig
    <$> loadUserConfigF sources
    <*> decodeThrow defaultConfigContent

loadUserConfigF
  :: (MonadUnliftIO m, MonadSystem m) => [ConfigSource] -> m (ConfigF Maybe)
loadUserConfigF = maybeM (pure emptyConfig) (uncurry decodeThrow') . readConfigSources

-- | @'decodeThrow'@, but wrapping YAML parse errors to @'ConfigError'@
decodeThrow' :: (MonadUnliftIO m, FromJSON a) => FilePath -> ByteString -> m a
decodeThrow' path content =
  handleTo (ConfigErrorInvalidYaml path content) $ decodeThrow content

decodeThrow :: (MonadIO m, FromJSON a) => ByteString -> m a
decodeThrow = either throwIO pure . Yaml.decodeThrow

-- | Populate @'cRestylers'@ using the versioned restylers data
--
-- May throw @'ConfigErrorInvalidRestylers'@.
resolveRestylers :: MonadIO m => ConfigF Identity -> [Restyler] -> m Config
resolveRestylers ConfigF {..} allRestylers = do
  restylers <-
    either (throwIO . ConfigErrorInvalidRestylers) pure
      $ overrideRestylers allRestylers
      $ unSketchy
      $ runIdentity cfRestylers

  pure
    Config
      { cEnabled = runIdentity cfEnabled
      , cExclude =
          Set.fromList $ unSketchy $ runIdentity $ cfExclude <> cfAlsoExclude
      , cChangedPaths = runIdentity cfChangedPaths
      , cAuto = runIdentity cfAuto
      , cCommitTemplate = runIdentity cfCommitTemplate
      , cRemoteFiles = unSketchy $ runIdentity cfRemoteFiles
      , cPullRequests = runIdentity cfPullRequests
      , cComments = runIdentity cfComments
      , cStatuses = runIdentity cfStatuses
      , cRequestReview = runIdentity cfRequestReview
      , cLabels = Set.fromList $ unSketchy $ runIdentity cfLabels
      , cIgnoreAuthors = unSketchy $ runIdentity cfIgnoreAuthors
      , cIgnoreBranches = unSketchy $ runIdentity cfIgnoreBranches
      , cIgnoreLabels = unSketchy $ runIdentity cfIgnoreLabels
      , cRestylers = restylers
      }

class HasConfig env where
  configL :: Lens' env Config

whenConfig
  :: (MonadReader env m, HasConfig env) => (Config -> Bool) -> m () -> m ()
whenConfig check act =
  whenConfigJust (bool Nothing (Just ()) . check) (const act)

whenConfigNonEmpty
  :: (MonadReader env m, HasConfig env)
  => (Config -> [a])
  -> ([a] -> m ())
  -> m ()
whenConfigNonEmpty check act =
  whenConfigJust (NE.nonEmpty . check) (act . NE.toList)

whenConfigJust
  :: (MonadReader env m, HasConfig env)
  => (Config -> Maybe a)
  -> (a -> m ())
  -> m ()
whenConfigJust check act = traverse_ act . check =<< view configL

defaultConfigContent :: ByteString
defaultConfigContent = $(embedFile "config/default.yaml")

configPaths :: [FilePath]
configPaths =
  [ ".restyled.yaml"
  , ".restyled.yml"
  , ".github/restyled.yaml"
  , ".github/restyled.yml"
  ]

handleTo
  :: (MonadUnliftIO m, Exception e1, Exception e2) => (e1 -> e2) -> m a -> m a
handleTo f = handle (throwIO . f)
