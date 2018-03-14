{-# LANGUAGE NoImplicitPrelude #-}
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

import ClassyPrelude

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Bifunctor (first)
import Data.Yaml
import Restyler.Config.Include
import Restyler.Config.Interpreter
import System.Directory (doesFileExist)

import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V

data Config = Config
    { cEnabled :: Bool
    , cRestylers :: [Restyler]
    }
    deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
    { cEnabled = True
    , cRestylers =
        [ unsafeNamedRestyler "stylish-haskell"
        , unsafeNamedRestyler "prettier"
        , unsafeNamedRestyler "shfmt"
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
        , rCommand = "brittany-inplace"
        , rArguments = []
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
    ]

namedRestyler :: MonadPlus m => Text -> m Restyler
namedRestyler name =
    case find ((== name) . pack . rName) allRestylers of
        Nothing -> fail $ unpack $ "Unknown restyler name: " <> name <> "."
        Just r -> pure r

unsafeNamedRestyler :: Text -> Restyler
unsafeNamedRestyler = either error id . namedRestyler

instance FromJSON Config where
    parseJSON (Array v) = Config
        <$> pure (cEnabled defaultConfig)
        <*> mapM parseJSON (V.toList v)
    parseJSON (Object o) = Config
        -- Use default values if un-specified
        <$> o .:? "enabled" .!= cEnabled defaultConfig
        <*> o .:? "restylers" .!= cRestylers defaultConfig
    parseJSON v = typeMismatch "Config object or list of restylers" v

-- | Load from @'configPath'@ if it exists, otherwise '@defaultConfig'
loadConfig :: IO (Either String Config)
loadConfig = doesFileExist configPath >>= \e -> if e
    then loadConfigFrom configPath
    else pure $ Right defaultConfig

loadConfigFrom :: FilePath -> IO (Either String Config)
loadConfigFrom fp = first err <$> decodeFileEither fp
  where
    err = (("Invalid configuration " <> show fp <> ": ") <>)
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
                    <$> pure (unpack k)
                    <*> o' .:? "command" .!= rCommand
                    <*> o' .:? "arguments" .!= rArguments
                    <*> o' .:? "include" .!= rInclude
                    <*> o' .:? "interpreters" .!= rInterpreters
                    <*> pure rSupportsArgSep
            ) v'
        _ -> typeMismatch "Name with override object" v
    parseJSON (String t) = namedRestyler t
    parseJSON v = typeMismatch "Name or named with override object" v

restylePaths :: Restyler -> [FilePath] -> IO [FilePath]
restylePaths Restyler{..} = filterM $ \path -> (||)
    <$> pure (includePath rInclude path)
    <*> anyM (path `hasInterpreter`) rInterpreters

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = pure False
anyM p (x:xs) = p x >>= \r ->
    if r then pure True else anyM p xs
