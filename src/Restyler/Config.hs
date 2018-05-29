{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Config
    ( Config(..)
    , configPath
    , StatusesConfig(..)
    , Restyler(..)

    -- * Exported for use in tests
    , defaultConfig
    , namedRestyler
    , unsafeNamedRestyler
    )
where

import Restyler.Prelude.NoApp

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import Restyler.Config.Include
import Restyler.Config.Interpreter

-- | Top-level configuration object
data Config = Config
    { cEnabled :: Bool
    -- ^ Do anything at all?
    , cAuto :: Bool
    -- ^ Just push the restyling, don't comment?
    , cStatusesConfig :: StatusesConfig
    -- ^ Send PR statuses?
    , cRestylers :: [Restyler]
    -- ^ What restylers to run
    }
    deriving (Eq, Show)

instance FromJSON Config where
    parseJSON (Array v) = do
        restylers <- mapM parseJSON (V.toList v)
        pure defaultConfig { cRestylers = restylers }
    parseJSON (Object o) = Config
        -- Use default values if un-specified
        <$> o .:? "enabled" .!= cEnabled defaultConfig
        <*> o .:? "auto" .!= cAuto defaultConfig
        <*> o .:? "statuses" .!= cStatusesConfig defaultConfig
        <*> o .:? "restylers" .!= cRestylers defaultConfig
    parseJSON v = typeMismatch "Config object or list of restylers" v

-- | @.restyled.yaml@
configPath :: FilePath
configPath = ".restyled.yaml"

-- | Default configuration
--
-- - Enabled
-- - Not Auto
-- - Send statuses
-- - Run most restylers
--
defaultConfig :: Config
defaultConfig = Config
    { cEnabled = True
    , cAuto = False
    , cStatusesConfig = defaultStatusesConfig
    , cRestylers = defaultRestylers
    }

-- | Configuration for sending PR statuses
data StatusesConfig = StatusesConfig
    { scDifferences :: Bool
    -- ^ Send a failure status when there were differences
    , scNoDifferences :: Bool
    -- ^ Send a success status when there were no differences
    , scError :: Bool
    -- ^ Send a failure status when there were errors
    }
    deriving (Eq, Show)

instance FromJSON StatusesConfig where
    parseJSON (Object o) = StatusesConfig
        <$> o .:? "differences" .!= scDifferences
        <*> o .:? "no-differences" .!= scNoDifferences
        <*> o .:? "error" .!= scError
      where
        StatusesConfig{..} = defaultStatusesConfig
    parseJSON (Aeson.Bool b) = pure StatusesConfig
        { scDifferences = b
        , scNoDifferences = b
        , scError = b
        }
    parseJSON x = typeMismatch "Boolean or Statuses Configuration" x

defaultStatusesConfig :: StatusesConfig
defaultStatusesConfig = StatusesConfig
    { scDifferences = True
    , scNoDifferences = True
    , scError = True
    }

-- | How to run a given restyler
data Restyler = Restyler
    { rName :: String
    -- ^ Unique name for this restyler, not configurable
    , rCommand :: String
    -- ^ Command to run, usually the name, not configurable
    , rArguments :: [String]
    -- ^ Arguments to pass before the paths
    , rInclude :: [Include]
    -- ^ Patterns to match for files to restyle
    , rInterpreters :: [Interpreter]
    -- ^ Interpreters to check for
    , rSupportsArgSep :: Bool
    -- ^ Can we pass @--@ between arguments and paths
    }
    deriving (Eq, Show)

instance FromJSON Restyler where
    parseJSON (String t) = namedRestyler t
    parseJSON v@(Object o) = case HM.toList o of
        [(k, v')] -> withObject "Override object"
            (\o' -> do
                Restyler{..} <- namedRestyler k
                Restyler (unpack k) rCommand -- Named + overrides
                    <$> o' .:? "arguments" .!= rArguments
                    <*> o' .:? "include" .!= rInclude
                    <*> o' .:? "interpreters" .!= rInterpreters
                    <*> pure rSupportsArgSep
            ) v'
        _ -> typeMismatch "Name with override object" v
    parseJSON v = typeMismatch "Name or named with override object" v

defaultRestylers :: [Restyler]
defaultRestylers =
    [ unsafeNamedRestyler "stylish-haskell"
    , unsafeNamedRestyler "prettier"
    , unsafeNamedRestyler "shfmt"
    , unsafeNamedRestyler "astyle"
    , unsafeNamedRestyler "autopep8"
    , unsafeNamedRestyler "php-cs-fixer"
    , unsafeNamedRestyler "elm-format"
    , unsafeNamedRestyler "rubocop"
    , unsafeNamedRestyler "rustfmt"
    ]

allRestylers :: [Restyler]
allRestylers =
    [ Restyler
        { rName = "stylish-haskell"
        , rCommand = "stylish-haskell"
        , rArguments = ["--inplace"]
        , rInclude = ["**/*.hs"]
        , rInterpreters = []
        , rSupportsArgSep = True
        }
    , Restyler
        { rName = "prettier"
        , rCommand = "prettier"
        , rArguments = ["--write"]
        , rInclude = ["**/*.js", "**/*.jsx"]
        , rInterpreters = []
        , rSupportsArgSep = True
        }
    , Restyler
        { rName = "hindent"
        , rCommand = "hindent-inplace"
        , rArguments = []
        , rInclude = ["**/*.hs"]
        , rInterpreters = []
        , rSupportsArgSep = True
        }
    , Restyler
        { rName = "brittany"
        , rCommand = "brittany"
        , rArguments = ["--write-mode", "inplace"]
        , rInclude = ["**/*.hs"]
        , rInterpreters = []
        , rSupportsArgSep = False
        }
    , Restyler
        { rName = "shfmt"
        , rCommand = "shfmt"
        , rArguments = ["-w"]
        , rInclude = ["**/*.sh", "**/*.bash"]
        , rInterpreters = [Sh, Bash]
        , rSupportsArgSep = True
        }
    , Restyler
        { rName = "astyle"
        , rCommand = "astyle"
        , rArguments = []
        , rInclude = [ "**/*.c" -- C
                     , "**/*.cpp" -- C++
                     , "**/*.cs" -- C#
                     , "**/*.h" -- C/C++/C#/Objective-C
                     , "**/*.java" -- Java
                     , "**/*.m" -- Objective-C
                     , "**/*.mm" -- Objective-C++
                     ]
        , rInterpreters = []
        , rSupportsArgSep = False
        }
    , Restyler
        { rName = "autopep8"
        , rCommand = "autopep8"
        , rArguments = ["--in-place"]
        , rInclude = ["**/*.py"]
        , rInterpreters = [Python]
        , rSupportsArgSep = True
        }
    , Restyler
        { rName = "php-cs-fixer"
        , rCommand = "php-cs-fixer-multi"
        , rArguments = []
        , rInclude = ["**/*.php"]
        , rInterpreters = []
        , rSupportsArgSep = True
        }
    , Restyler
        { rName = "elm-format"
        , rCommand = "elm-format"
        , rArguments = ["--yes"]
        , rInclude = ["**/*.elm"]
        , rInterpreters = []
        , rSupportsArgSep = True
        }
    , Restyler
        { rName = "rubocop"
        , rCommand = "rubocop"
        , rArguments = ["--auto-correct"]
        , rInclude = ["**/*.rb"]
        , rInterpreters = [Ruby]
        , rSupportsArgSep = True
        }
    , Restyler
        { rName = "rustfmt"
        , rCommand = "rustfmt"
        , rArguments = []
        , rInclude = ["**/*.rs"]
        , rInterpreters = []
        , rSupportsArgSep = True
        }
    ]

namedRestyler :: MonadPlus m => Text -> m Restyler
namedRestyler name = case find ((== name) . pack . rName) allRestylers of
    Nothing -> fail $ unpack $ "Unknown restyler name: " <> name <> "."
    Just r -> pure r

unsafeNamedRestyler :: Text -> Restyler
unsafeNamedRestyler = either error id . namedRestyler
