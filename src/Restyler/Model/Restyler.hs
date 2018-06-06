{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Restyler.Model.Restyler
    ( Restyler(..)
    , defaultRestylers
    , allRestylers
    , namedRestyler
    , unsafeNamedRestyler

    -- * Running
    , runRestylers
    )
where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.HashMap.Lazy as HM
import Restyler.Capabilities.Docker
import Restyler.Capabilities.System
import Restyler.Model.Include
import Restyler.Model.Interpreter

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
        , rArguments = ["--auto-correct", "--fail-level", "fatal"]
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

runRestylers
    :: (Monad m, MonadSystem m, MonadDocker m, MonadLogger m)
    => [Restyler]
    -> [FilePath]
    -> m ()
runRestylers restylers allPaths = do
    cwd <- getCurrentDirectory
    existingPaths <- filterM doesFileExist allPaths

    logDebugN $ "Restylers: " <> tshow (map rName restylers)
    logDebugN $ "Paths: " <> tshow existingPaths

    for_ restylers $ \r@Restyler {..} -> do
        paths <- filterM (shouldInclude r) existingPaths

        unless (null paths) $ do
            logInfoN $ "Restyling " <> tshow paths <> " via " <> pack rName
            dockerRun
                $ runOptions cwd rName rCommand
                <> rArguments
                <> pathArguments rSupportsArgSep paths

shouldInclude :: (Monad m, MonadSystem m) => Restyler -> FilePath -> m Bool
shouldInclude Restyler {..} path
    | includePath rInclude path = pure True
    | otherwise = do
        contents <- readFile path
        pure $ any (contents `hasInterpreter`) rInterpreters

pathArguments :: Bool -> [FilePath] -> [String]
pathArguments True = ("--" :) . map prependIfRelative
pathArguments False = map prependIfRelative

-- | Some Restylers won't assume @foo@ means @.\/foo@, unless told
prependIfRelative :: FilePath -> FilePath
prependIfRelative path
    | any (`isPrefixOf` path) ["/", "./", "../"] = path
    | otherwise = "./" <> path

-- brittany-disable-next-binding
runOptions :: FilePath -> String -> String -> [String]
runOptions dir name command =
    [ "--rm", "--net", "none"
    , "--volume", dir <> ":/code"
    , "restyled/restyler-" <> name, command
    ]
