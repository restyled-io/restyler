{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Config
    ( Config(..)
    , loadConfig
    , loadConfigFrom
    , Restyler(..)
    , restylePaths

    -- * Include Patterns
    , module Restyler.Config.Include

    -- * Filtering by shebang
    , module Restyler.Config.Interpreter

    -- * Exported for documentation only
    , configPath
    , defaultConfig
    , allRestylers

    -- * Exported for use in tests
    , namedRestyler
    , unsafeNamedRestyler
    ) where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Bifunctor (first)
import qualified Data.HashMap.Lazy as HM
import Data.List (find)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Yaml
import Restyler.Config.Include
import Restyler.Config.Interpreter
import System.Directory (doesFileExist)

data Config = Config
    { cEnabled :: Bool
    , cAuto :: Bool
    , cRestylers :: [Restyler]
    }
    deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
    { cEnabled = True
    , cAuto = False
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
namedRestyler name = case find ((== name) . T.pack . rName) allRestylers of
    Nothing -> fail $ T.unpack $ "Unknown restyler name: " <> name <> "."
    Just r -> pure r

unsafeNamedRestyler :: Text -> Restyler
unsafeNamedRestyler = either error id . namedRestyler

instance FromJSON Config where
    parseJSON (Array v) = do
        restylers <- mapM parseJSON (V.toList v)
        pure defaultConfig { cRestylers = restylers }
    parseJSON (Object o) = Config
        -- Use default values if un-specified
        <$> o .:? "enabled" .!= cEnabled defaultConfig
        <*> o .:? "auto" .!= cAuto defaultConfig
        <*> o .:? "restylers" .!= cRestylers defaultConfig
    parseJSON v = typeMismatch "Config object or list of restylers" v

-- | Load from @'configPath'@ if it exists, otherwise '@defaultConfig'
loadConfig :: IO (Either String Config)
loadConfig = doesFileExist configPath >>= \e ->
    if e then loadConfigFrom configPath else pure $ Right defaultConfig

loadConfigFrom :: FilePath -> IO (Either String Config)
loadConfigFrom fp = first err <$> decodeFileEither fp
  where
    err =
        (("Invalid configuration " <> show fp <> ": ") <>)
            . prettyPrintParseException

configPath :: FilePath
configPath = ".restyled.yaml"

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
                    <$> pure (T.unpack k)
                    <*> pure rCommand
                    <*> o' .:? "arguments" .!= rArguments
                    <*> o' .:? "include" .!= rInclude
                    <*> o' .:? "interpreters" .!= rInterpreters
                    <*> pure rSupportsArgSep
            ) v'
        _ -> typeMismatch "Name with override object" v
    parseJSON (String t) = namedRestyler t
    parseJSON v = typeMismatch "Name or named with override object" v

restylePaths :: Restyler -> [FilePath] -> IO [FilePath]
restylePaths Restyler {..} = filterM $ \path ->
    (||)
        <$> pure (includePath rInclude path)
        <*> anyM (path `hasInterpreter`) rInterpreters

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = pure False
anyM p (x : xs) = p x >>= \r -> if r then pure True else anyM p xs
