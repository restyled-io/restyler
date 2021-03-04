module Restyler.App.Setup
    ( App
    , loadApp
    ) where

import Restyler.Prelude

import GitHub.Endpoints.PullRequests
import qualified Restyler.App.Startup as Startup
import Restyler.Capabilities.GitHub
import Restyler.Options
import Restyler.PullRequest
import Restyler.WorkingDirectory

data App = App
    { appStartupApp :: Startup.App
    , appPullRequest :: PullRequest
    }

startupAppL :: Lens' App Startup.App
startupAppL = lens appStartupApp $ \x y -> x { appStartupApp = y }

instance HasLogFunc App where
    logFuncL = startupAppL . logFuncL

instance HasOptions App where
    optionsL = startupAppL . optionsL

instance HasWorkingDirectory App where
    workingDirectoryL = startupAppL . workingDirectoryL

instance HasPullRequest App where
    pullRequestL = lens appPullRequest $ \x y -> x { appPullRequest = y }

loadApp
    :: (MonadGitHub m, MonadReader env m, HasOptions env)
    => Startup.App
    -> m App
loadApp app = do
    Options {..} <- view optionsL
    App app <$> getPullRequest oOwner oRepo oPullRequest
