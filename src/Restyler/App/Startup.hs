module Restyler.App.Startup
    ( App
    , loadApp
    ) where

import Restyler.Prelude

import Restyler.Logger
import Restyler.Options
import Restyler.WorkingDirectory

data App = App
    { appLogFunc :: LogFunc
    , appOptions :: Options
    , appWorkingDirectory :: FilePath
    }

instance HasLogFunc App where
    logFuncL = lens appLogFunc $ \x y -> x { appLogFunc = y }

instance HasOptions App where
    optionsL = lens appOptions $ \x y -> x { appOptions = y }

instance HasWorkingDirectory App where
    workingDirectoryL =
        lens appWorkingDirectory $ \x y -> x { appWorkingDirectory = y }

loadApp :: Options -> FilePath -> App
loadApp options path = App
    { appLogFunc = restylerLogFunc options
    , appOptions = options
    , appWorkingDirectory = path
    }
