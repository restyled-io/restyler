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

data Config = Config
    { cEnabled :: Bool
    , cAuto :: Bool
    , cStatusesConfig :: StatusesConfig
    , cRestylers :: [Restyler]
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

configPath :: FilePath
configPath = ".restyled.yaml"

defaultConfig :: Config
defaultConfig = Config
    { cEnabled = True
    , cAuto = False
    , cStatusesConfig = defaultStatusesConfig
    , cRestylers = [ unsafeNamedRestyler "stylish-haskell"
                   , unsafeNamedRestyler "prettier"
                   , unsafeNamedRestyler "shfmt"
                   , unsafeNamedRestyler "astyle"
                   , unsafeNamedRestyler "autopep8"
                   , unsafeNamedRestyler "php-cs-fixer"
                   , unsafeNamedRestyler "elm-format"
                   , unsafeNamedRestyler "rubocop"
                   , unsafeNamedRestyler "rustfmt"
                   ]
    }

data StatusesConfig = StatusesConfig
    { scDifferences :: Bool
    , scNoDifferences :: Bool
    , scError :: Bool
    }
    deriving (Eq, Show)

instance FromJSON StatusesConfig where
    parseJSON (Object o) = do
        let StatusesConfig{..} = defaultStatusesConfig
        StatusesConfig
            <$> o .:? "differences" .!= scDifferences
            <*> o .:? "no-differences" .!= scNoDifferences
            <*> o .:? "error" .!= scError
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

data Restyler = Restyler
    { rName :: String
    , rCommand :: String
    , rArguments :: [String]
    , rInclude :: [Include]
    , rInterpreters :: [Interpreter]
    , rSupportsArgSep :: Bool
    }
    deriving (Eq, Show)

instance FromJSON Restyler where
    parseJSON v@(Object o) = case HM.toList o of
        [(k, v')] -> withObject "Override object"
            (\o' -> do
                Restyler{..} <- namedRestyler k
                Restyler -- Named + overrides
                    <$> pure (unpack k)
                    <*> pure rCommand
                    <*> o' .:? "arguments" .!= rArguments
                    <*> o' .:? "include" .!= rInclude
                    <*> o' .:? "interpreters" .!= rInterpreters
                    <*> pure rSupportsArgSep
            ) v'
        _ -> typeMismatch "Name with override object" v
    parseJSON (String t) = namedRestyler t
    parseJSON v = typeMismatch "Name or named with override object" v

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
