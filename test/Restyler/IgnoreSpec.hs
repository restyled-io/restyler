module Restyler.IgnoreSpec
    ( spec
    )
where

import SpecHelper

import qualified Data.Set as Set
import Restyler.Config
import Restyler.Ignore
import qualified RIO.Vector as V

spec :: Spec
spec = do
    describe "ignoreByLabels" $ do
        it "checks labels against configuration" $ do
            config <- loadModifiedConfig
                $ \c -> c { cIgnoreLabels = Set.fromList ["restyler-ignore"] }

            ignoreByLabels config (V.fromList []) `shouldBe` Nothing
            ignoreByLabels config (V.fromList ["wip", "debug"])
                `shouldBe` Nothing
            ignoreByLabels config (V.fromList ["restyler-", "ignore"])
                `shouldBe` Nothing
            ignoreByLabels config (V.fromList ["restyler-ignore"])
                `shouldSatisfy` isJust

loadModifiedConfig :: (Config -> Config) -> IO Config
loadModifiedConfig f = f <$> runSimpleApp loadDefaultConfig
