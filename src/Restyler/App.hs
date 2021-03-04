module Restyler.App
    ( App
    , loadApp
    ) where

import Restyler.Prelude

import Restyler.App.Error
import qualified Restyler.App.Setup as Setup
import Restyler.Capabilities.DownloadFile
import Restyler.Capabilities.Git
import Restyler.Capabilities.Logger
import Restyler.Capabilities.System
import Restyler.Clone
import Restyler.Config
import Restyler.Options
import Restyler.PullRequest
import Restyler.WorkingDirectory

data App = App
    { appSetupApp :: Setup.App
    , appConfig :: Config
    }

setupAppL :: Lens' App Setup.App
setupAppL = lens appSetupApp $ \x y -> x { appSetupApp = y }

instance HasLogFunc App where
    logFuncL = setupAppL . logFuncL

instance HasOptions App where
    optionsL = setupAppL . optionsL

instance HasWorkingDirectory App where
    workingDirectoryL = setupAppL . workingDirectoryL

instance HasPullRequest App where
    pullRequestL = setupAppL . pullRequestL

instance HasConfig App where
    configL = lens appConfig $ \x y -> x { appConfig = y }

loadApp
    :: ( MonadLogger m
       , MonadSystem m
       , MonadDownloadFile m
       , MonadGit m
       , MonadError AppError m
       , MonadReader env m
       , HasOptions env
       , HasWorkingDirectory env
       , HasPullRequest env
       )
    => Setup.App
    -> m App
loadApp app = do
    clonePullRequest
    checkoutPullRequest
    App app <$> loadConfig
