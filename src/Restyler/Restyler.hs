module Restyler.Restyler
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
import Restyler.Config.ExpectedKeys
import Restyler.Config.Include
import Restyler.Config.Interpreter

-- | How to run a given restyler
data Restyler = Restyler
    { rName :: String
    -- ^ Unique name for this restyler, not configurable
    , rImage :: String
    -- ^ Docker image for this restyler, not configurable
    , rCommand :: [String]
    -- ^ Command to run, usually the name, not configurable
    --
    -- If there are arguments that are __always__ required, such as a
    -- sub-command or an inplace flag, they should be present here. This is to
    -- avoid users accidentally overriding them out and being confused.
    --
    , rDocumentation :: [String]
    -- ^ Documentation URLs for this Restyler, not configurable
    --
    -- These are showing if we encounter an error with this Restyler
    --
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

-- | Only those attributes that are settable in user configuration
data RestylerOverrides = RestylerOverrides
    { roArguments :: Maybe [String]
    , roInclude :: Maybe [Include]
    , roInterpreters :: Maybe [Interpreter]
    }

instance FromJSON RestylerOverrides where
    parseJSON = withObject "Restyler" $ \o -> do
        validateObjectKeys ["arguments", "include", "interpreters"] o
        RestylerOverrides
            <$> o .:? "arguments"
            <*> o .:? "include"
            <*> o .:? "interpreters"

fromRestylerOverrides :: MonadPlus m => Text -> RestylerOverrides -> m Restyler
fromRestylerOverrides name RestylerOverrides {..} = do
    restyler <- namedRestyler name
    pure restyler
        { rArguments = fromMaybe (rArguments restyler) roArguments
        , rInclude = fromMaybe (rInclude restyler) roInclude
        , rInterpreters = fromMaybe (rInterpreters restyler) roInterpreters
        }

instance FromJSON Restyler where
    parseJSON (String t) = namedRestyler t
    parseJSON v@(Object o) = case HM.toList o of
        [(k, v')] -> fromRestylerOverrides k =<< parseJSON v'
        _ -> typeMismatch "Name with override object" v
    parseJSON v = typeMismatch "Name or named with override object" v

instance ToJSON Restyler where
    toJSON Restyler{..} = object
        [ pack rName .= object
            [ "arguments" .= rArguments
            , "include" .= rInclude
            , "interpreters" .= rInterpreters
            , "internal" .= object
                [ "image" .= rImage
                , "command" .= rCommand
                , "supports_arg_sep" .= rSupportsArgSep
                , "supports_multiple_paths" .= rSupportsMultiplePaths
                ]
            ]
        ]

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
    [ (baseRestyler "stylish-haskell")
        { rImage = "restyled/restyler-stylish-haskell:c0ba83d"
        , rCommand = ["stylish-haskell", "--inplace"]
        , rDocumentation = [wiki "/Common-Errors:-Stylish-Haskell"]
        , rInclude = ["**/*.hs"]
        }
    , (baseRestyler "prettier")
        { rImage = "restyled/restyler-prettier:8cd304f"
        , rCommand = ["prettier", "--write"]
        , rInclude = ["**/*.js", "**/*.jsx", "**/*.yml", "**/*.yaml"]
        }
    , (baseRestyler "hindent")
        { rInclude = ["**/*.hs"]
        , rSupportsArgSep = False
        , rSupportsMultiplePaths = False
        }
    , (baseRestyler "brittany")
        { rImage = "restyled/restyler-brittany:caa75f3"
        , rCommand = ["brittany", "--write-mode", "inplace"]
        , rDocumentation = [wiki "/Common-Errors:-Brittany"]
        , rInclude = ["**/*.hs"]
        , rSupportsArgSep = False
        }
    , (baseRestyler "shfmt")
        { rImage = "restyled/restyler-shfmt:95f0cfe"
        , rCommand = ["shfmt", "-w"]
        , rInclude = ["**/*.sh", "**/*.bash"]
        , rInterpreters = [Sh, Bash]
        }
    , (baseRestyler "astyle")
        { rInclude =
            [ "**/*.c" -- C
            , "**/*.cpp" -- C++
            , "**/*.cs" -- C#
            , "**/*.h" -- C/C++/C#/Objective-C
            --, "**/*.java" -- Leave Java to google-java-format
            , "**/*.m" -- Objective-C
            , "**/*.mm" -- Objective-C++
            ]
        , rSupportsArgSep = False
        }
    , (baseRestyler "autopep8")
        { rCommand = ["autopep8", "--in-place"]
        , rInclude = ["**/*.py"]
        , rInterpreters = [Python]
        }
    , (baseRestyler "php-cs-fixer")
        { rCommand = ["php-cs-fixer", "fix"]
        , rImage = "restyled/restyler-php-cs-fixer:1bda900"
        , rInclude = ["**/*.php"]
        , rSupportsMultiplePaths = False
        }
    , (baseRestyler "elm-format")
        { rCommand = ["elm-format", "--yes"]
        , rInclude = ["**/*.elm"]
        }
    , (baseRestyler "rubocop")
        -- required to operate correctly
        { rCommand = ["rubocop", "--auto-correct"]
        -- almost certainly desired, but not necessarily
        , rArguments = ["--fail-level", "fatal"]
        , rInclude = ["**/*.rb"]
        , rInterpreters = [Ruby]
        }
    , (baseRestyler "rustfmt") { rInclude = ["**/*.rs"] }
    , (baseRestyler "terraform")
        { rCommand = ["terraform", "fmt"]
        , rDocumentation = [wiki "/Common-Errors:-Rustfmt"]
        , rInclude = ["**/*.tf"]
        , rSupportsMultiplePaths = False
        }
    , (baseRestyler "dfmt")
        { rImage = "restyled/restyler-dfmt:f3b58e0"
        , rCommand = ["dfmt", "--inplace"]
        , rInclude = ["**/*.d"]
        }
    , (baseRestyler "google-java-format")
        { rImage = "restyled/restyler-google-java-format:39b1d8f"
        , rCommand = ["google-java-format", "--replace"]
        , rInclude = ["**/*.java"]
        , rSupportsArgSep = False
        }
    , (baseRestyler "hlint")
        { rImage = "restyled/restyler-hlint:efbd99c"
        , rCommand = ["hlint", "lint", "--refactor", "--refactor-options=-i"]
        , rInclude = ["**/*.hs"]
        , rSupportsMultiplePaths = False
        , rSupportsArgSep = False
        }
    , (baseRestyler "pg_format")
        { rImage = "restyled/restyler-pg_format:9862f49"
        , rCommand = ["pg_format-inplace"]
        , rInclude = ["**/*.sql"]
        , rSupportsArgSep = False
        , rSupportsMultiplePaths = False
        }
    ]
    where wiki = ("https://github.com/restyled-io/restyled.io/wiki" <>)

namedRestyler :: MonadPlus m => Text -> m Restyler
namedRestyler =
    either fail pure
        . validateExpectedKeyBy "restyler" rName allRestylers
        . unpack

unsafeNamedRestyler :: Text -> Restyler
unsafeNamedRestyler = either error id . namedRestyler

baseRestyler :: String -> Restyler
baseRestyler name = Restyler
    { rName = name
    , rImage = "restyled/restyler-" <> name
    , rCommand = [name]
    , rDocumentation = []
    , rArguments = []
    , rInclude = ["**/*"]
    , rInterpreters = []
    , rSupportsArgSep = True
    , rSupportsMultiplePaths = True
    }
