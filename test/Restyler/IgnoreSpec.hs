module Restyler.IgnoreSpec
    ( spec
    )
where

import SpecHelper

import Restyler.Config
import Restyler.Config.Glob
import Restyler.Ignore

spec :: Spec
spec = do
    describe "ignoreByLabels" $ do
        it "works in empty state" $ do
            config <- loadModifiedConfig $ \c -> c { cIgnoreLabels = [] }
            ignoreByLabels config [] `shouldBe` Nothing

        it "checks labels against configuration" $ do
            config <- loadModifiedConfig
                $ \c -> c { cIgnoreLabels = [Glob "wip"] }

            ignoreByLabels config [] `shouldBe` Nothing
            ignoreByLabels config ["other"] `shouldBe` Nothing
            ignoreByLabels config ["wip"] `shouldSatisfy` isJust

        it "checks any config against any label" $ do
            config <- loadModifiedConfig
                $ \c -> c { cIgnoreLabels = [Glob "wip", Glob "debug"] }

            ignoreByLabels config ["one", "debug"] `shouldSatisfy` isJust
            ignoreByLabels config ["wip", "two"] `shouldSatisfy` isJust

        it "supports globs" $ do
            config <- loadModifiedConfig
                $ \c -> c { cIgnoreLabels = [Glob "beta/*"] }

            ignoreByLabels config ["beta/this"] `shouldSatisfy` isJust
            ignoreByLabels config ["beta/that"] `shouldSatisfy` isJust

loadModifiedConfig :: (Config -> Config) -> IO Config
loadModifiedConfig f = f <$> runSimpleApp loadDefaultConfig
