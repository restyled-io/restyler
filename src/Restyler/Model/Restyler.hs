{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Model.Restyler
    ( Restyler(..)
    , defaultRestylers
    , allRestylers
    , namedRestyler
    , unsafeNamedRestyler
    ) where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.HashMap.Lazy as HM
import Restyler.Model.Include
import Restyler.Model.Interpreter

-- | How to run a given restyler
data Restyler = Restyler
    { rName :: String
    -- ^ Unique name for this restyler, not configurable
    , rImage :: String
    -- ^ Docker image for this restyler, not configurable
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
    , rSupportsMultiplePaths :: Bool
    -- ^ Can we pass multiple paths at once
    }
    deriving (Eq, Show)

instance FromJSON Restyler where
    parseJSON (String t) = namedRestyler t
    parseJSON v@(Object o) = case HM.toList o of
        [(k, v')] -> withObject "Override object"
            (\o' -> do
                Restyler{..} <- namedRestyler k
                Restyler (unpack k) rImage rCommand -- Named + overrides
                    <$> o' .:? "arguments" .!= rArguments
                    <*> o' .:? "include" .!= rInclude
                    <*> o' .:? "interpreters" .!= rInterpreters
                    <*> pure rSupportsArgSep
                    <*> pure rSupportsMultiplePaths
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
    , unsafeNamedRestyler "terraform"
    ]

allRestylers :: [Restyler]
allRestylers =
    [ Restyler
        { rName = "stylish-haskell"
        , rImage = "restyled/restyler-stylish-haskell:c0ba83d"
        , rCommand = "stylish-haskell"
        , rArguments = ["--inplace"]
        , rInclude = ["**/*.hs"]
        , rInterpreters = []
        , rSupportsArgSep = True
        , rSupportsMultiplePaths = True
        }
    , Restyler
        { rName = "prettier"
        , rImage = "restyled/restyler-prettier"
        , rCommand = "prettier"
        , rArguments = ["--write"]
        , rInclude = ["**/*.js", "**/*.jsx"]
        , rInterpreters = []
        , rSupportsArgSep = True
        , rSupportsMultiplePaths = True
        }
    , Restyler
        { rName = "hindent"
        , rImage = "restyled/restyler-hindent"
        , rCommand = "hindent"
        , rArguments = []
        , rInclude = ["**/*.hs"]
        , rInterpreters = []
        , rSupportsArgSep = False
        , rSupportsMultiplePaths = False
        }
    , Restyler
        { rName = "brittany"
        , rImage = "restyled/restyler-brittany"
        , rCommand = "brittany"
        , rArguments = ["--write-mode", "inplace"]
        , rInclude = ["**/*.hs"]
        , rInterpreters = []
        , rSupportsArgSep = False
        , rSupportsMultiplePaths = True
        }
    , Restyler
        { rName = "shfmt"
        , rImage = "restyled/restyler-shfmt"
        , rCommand = "shfmt"
        , rArguments = ["-w"]
        , rInclude = ["**/*.sh", "**/*.bash"]
        , rInterpreters = [Sh, Bash]
        , rSupportsArgSep = True
        , rSupportsMultiplePaths = True
        }
    , Restyler
        { rName = "astyle"
        , rImage = "restyled/restyler-astyle"
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
        , rSupportsMultiplePaths = True
        }
    , Restyler
        { rName = "autopep8"
        , rImage = "restyled/restyler-autopep8"
        , rCommand = "autopep8"
        , rArguments = ["--in-place"]
        , rInclude = ["**/*.py"]
        , rInterpreters = [Python]
        , rSupportsArgSep = True
        , rSupportsMultiplePaths = True
        }
    , Restyler
        { rName = "php-cs-fixer"
        , rImage = "restyled/restyler-php-cs-fixer"
        , rCommand = "php-cs-fixer"
        , rArguments = ["fix"]
        , rInclude = ["**/*.php"]
        , rInterpreters = []
        , rSupportsArgSep = True
        , rSupportsMultiplePaths = False
        }
    , Restyler
        { rName = "elm-format"
        , rImage = "restyled/restyler-elm-format"
        , rCommand = "elm-format"
        , rArguments = ["--yes"]
        , rInclude = ["**/*.elm"]
        , rInterpreters = []
        , rSupportsArgSep = True
        , rSupportsMultiplePaths = True
        }
    , Restyler
        { rName = "rubocop"
        , rImage = "restyled/restyler-rubocop"
        , rCommand = "rubocop"
        , rArguments = ["--auto-correct", "--fail-level", "fatal"]
        , rInclude = ["**/*.rb"]
        , rInterpreters = [Ruby]
        , rSupportsArgSep = True
        , rSupportsMultiplePaths = True
        }
    , Restyler
        { rName = "rustfmt"
        , rImage = "restyled/restyler-rustfmt"
        , rCommand = "rustfmt"
        , rArguments = []
        , rInclude = ["**/*.rs"]
        , rInterpreters = []
        , rSupportsArgSep = True
        , rSupportsMultiplePaths = True
        }
    , Restyler
        { rName = "terraform"
        , rImage = "restyled/restyler-terraform"
        , rCommand = "terraform"
        , rArguments = ["fmt"]
        , rInclude = ["**/*.tf"]
        , rInterpreters = []
        , rSupportsArgSep = True
        , rSupportsMultiplePaths = False
        }
    ]

namedRestyler :: MonadPlus m => Text -> m Restyler
namedRestyler name = case find ((== name) . pack . rName) allRestylers of
    Nothing -> fail $ unpack $ "Unknown restyler name: " <> name <> "."
    Just r -> pure r

unsafeNamedRestyler :: Text -> Restyler
unsafeNamedRestyler = either error id . namedRestyler
