{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
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
module Restyler.Config
    ( Config(..)
    , ConfigError(..)
    , configPullRequestReviewer
    , configRestyledRef
    , loadConfig
    , HasConfig(..)
    , whenConfig
    , whenConfigNonEmpty
    , whenConfigJust

    -- * Exported for use in tests
    , ConfigSource(..)
    , loadConfigFrom
    , resolveRestylers
    , defaultConfigContent
    )
where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Casing
import qualified Data.ByteString.Char8 as C8
import Data.FileEmbed (embedFile)
import Data.Functor.Barbie
import Data.List (isInfixOf)
import qualified Data.List.NonEmpty as NE
import Data.Monoid (Alt(..))
import qualified Data.Set as Set
import Data.Yaml (decodeThrow)
import qualified Data.Yaml as Yaml
import qualified Data.Yaml.Ext as Yaml
import GitHub.Data (IssueLabel, User)
import Restyler.App.Class
import Restyler.Config.BranchName
import Restyler.Config.ExpectedKeys
import Restyler.Config.Glob
import Restyler.Config.RequestReview
import Restyler.Config.Restyler
import Restyler.Config.SketchyList
import Restyler.Config.Statuses
import Restyler.PullRequest
import Restyler.RemoteFile
import Restyler.Restyler

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
--
data ConfigF f = ConfigF
    { cfEnabled :: f Bool
    , cfExclude :: f (SketchyList Glob)
    , cfAuto :: f Bool
    , cfRemoteFiles :: f (SketchyList RemoteFile)
    , cfPullRequests :: f Bool
    , cfBranchName :: f BranchName
    , cfComments :: f Bool
    , cfStatuses :: f Statuses
    , cfRequestReview :: f RequestReviewConfig
    , cfLabels :: f (SketchyList (Name IssueLabel))
    , cfIgnoreLabels :: f (SketchyList (Name IssueLabel))
    , cfRestylers :: f (Maybe (SketchyList RestylerOverride))
    , cfRestylersVersion :: f String
    }
    deriving stock Generic
    deriving anyclass (FunctorB, ApplicativeB, ConstraintsB)

-- | An empty @'ConfigF'@ of all @'Nothing'@s
--
-- N.B. the choice of @'getAlt'@ is somewhat arbitrary. We just need a @Maybe@
-- wrapper @f a@ where @getX mempty@ is @Nothing@, but without a @Monoid a@
-- constraint.
--
emptyConfig :: ConfigF Maybe
emptyConfig = bmap getAlt bmempty

instance FromJSON (ConfigF Maybe) where
    parseJSON a@(Array _) = do
        restylers <- parseJSON a
        pure emptyConfig { cfRestylers = Just restylers }
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
--
data Config = Config
    { cEnabled :: Bool
    , cExclude :: [Glob]
    , cAuto :: Bool
    , cRemoteFiles :: [RemoteFile]
    , cPullRequests :: Bool
    , cBranchName :: BranchName
    , cComments :: Bool
    , cStatuses :: Statuses
    , cRequestReview :: RequestReviewConfig
    , cLabels :: Set (Name IssueLabel)
    , cIgnoreLabels :: Set (Name IssueLabel)
    , cRestylers :: [Restyler]
    -- ^ TODO: @'NonEmpty'@
    --
    -- It's true, but what's the benefit?
    --
    }
    deriving (Eq, Show, Generic)

-- | If so configured, return the @'User'@ from whom to request review
configPullRequestReviewer :: PullRequest -> Config -> Maybe (Name User)
configPullRequestReviewer pr = determineReviewer pr . cRequestReview

configRestyledRef :: PullRequest -> Config -> Text
configRestyledRef pr Config {..} =
    interpolateBranchName cBranchName $ pullRequestHeadRef pr

instance ToJSON Config where
    toJSON = genericToJSON $ aesonPrefix snakeCase
    toEncoding = genericToEncoding $ aesonPrefix snakeCase

data ConfigError
    = ConfigErrorInvalidYaml ByteString Yaml.ParseException
    | ConfigErrorUnknownRestylers [String]
    | ConfigErrorInvalidRestylersYaml SomeException
    deriving Show

configErrorInvalidYaml :: ByteString -> Yaml.ParseException -> ConfigError
configErrorInvalidYaml yaml = ConfigErrorInvalidYaml yaml
    . Yaml.modifyYamlProblem modify
  where
    modify msg
        | isCannotStart msg && hasTabIndent yaml
        = msg
            <> "\n\nThis may be caused by your source file containing tabs."
            <> "\nYAML forbids tabs for indentation. See https://yaml.org/faq.html."
        | otherwise
        = msg
    isCannotStart = ("character that cannot start any token" `isInfixOf`)
    hasTabIndent = ("\n\t" `C8.isInfixOf`)

instance Exception ConfigError

-- | Load a fully-inflated @'Config'@
--
-- Read any @.restyled.yaml@, fill it out from defaults, grab the versioned set
-- of restylers data, and apply the configured choices and overrides.
--
loadConfig
    :: (HasLogFunc env, HasSystem env, HasDownloadFile env) => RIO env Config
loadConfig =
    loadConfigFrom (ConfigPath configPath)
        $ handleTo ConfigErrorInvalidRestylersYaml
        . getAllRestylersVersioned
        . runIdentity
        . cfRestylersVersion

loadConfigFrom
    :: HasSystem env
    => ConfigSource
    -> (ConfigF Identity -> RIO env [Restyler])
    -> RIO env Config
loadConfigFrom source f = do
    config <- loadConfigF source
    restylers <- f config
    resolveRestylers config restylers

data ConfigSource
    = ConfigPath FilePath
    | ConfigContent ByteString

readConfigSource :: HasSystem env => ConfigSource -> RIO env (Maybe ByteString)
readConfigSource = \case
    ConfigPath path -> do
        exists <- doesFileExist path
        if exists then Just <$> readFileBS path else pure Nothing
    ConfigContent content -> pure $ Just content

-- | Load configuration if present and apply defaults
--
-- Returns @'ConfigF' 'Identity'@ because defaulting has populated all fields.
--
-- May throw any @'ConfigError'@. May through raw @'Yaml.ParseException'@s if
-- there is a programmer error in our static default configuration YAML.
--
loadConfigF :: HasSystem env => ConfigSource -> RIO env (ConfigF Identity)
loadConfigF source =
    resolveConfig
        <$> loadUserConfigF source
        <*> decodeThrow defaultConfigContent

loadUserConfigF :: HasSystem env => ConfigSource -> RIO env (ConfigF Maybe)
loadUserConfigF = maybeM (pure emptyConfig) decodeThrow' . readConfigSource

-- | @'decodeThrow'@, but wrapping YAML parse errors to @'ConfigError'@
decodeThrow' :: (MonadUnliftIO m, MonadThrow m, FromJSON a) => ByteString -> m a
decodeThrow' content =
    handleTo (configErrorInvalidYaml content) $ decodeThrow content

-- | Populate @'cRestylers'@ using the versioned restylers data
--
-- May throw @'ConfigErrorUnknownRestylers'@.
--
resolveRestylers :: ConfigF Identity -> [Restyler] -> RIO env Config
resolveRestylers ConfigF {..} allRestylers = do
    restylers <-
        eitherM (throwIO . ConfigErrorUnknownRestylers) pure
        $ pure
        $ overrideRestylers allRestylers
        $ unSketchy
        <$> runIdentity cfRestylers

    pure Config
        { cEnabled = runIdentity cfEnabled
        , cExclude = unSketchy $ runIdentity cfExclude
        , cAuto = runIdentity cfAuto
        , cRemoteFiles = unSketchy $ runIdentity cfRemoteFiles
        , cPullRequests = runIdentity cfPullRequests
        , cBranchName = runIdentity cfBranchName
        , cComments = runIdentity cfComments
        , cStatuses = runIdentity cfStatuses
        , cRequestReview = runIdentity cfRequestReview
        , cLabels = Set.fromList $ unSketchy $ runIdentity cfLabels
        , cIgnoreLabels = Set.fromList $ unSketchy $ runIdentity cfIgnoreLabels
        , cRestylers = restylers
        }

class HasConfig env where
    configL :: Lens' env Config

whenConfig :: HasConfig env => (Config -> Bool) -> RIO env () -> RIO env ()
whenConfig check act =
    whenConfigJust (bool Nothing (Just ()) . check) (const act)

whenConfigNonEmpty
    :: HasConfig env => (Config -> [a]) -> ([a] -> RIO env ()) -> RIO env ()
whenConfigNonEmpty check act =
    whenConfigJust (NE.nonEmpty . check) (act . NE.toList)

whenConfigJust
    :: HasConfig env => (Config -> Maybe a) -> (a -> RIO env ()) -> RIO env ()
whenConfigJust check act = traverse_ act . check =<< view configL

defaultConfigContent :: ByteString
defaultConfigContent = $(embedFile "config/default.yaml")

configPath :: FilePath
configPath = ".restyled.yaml"
