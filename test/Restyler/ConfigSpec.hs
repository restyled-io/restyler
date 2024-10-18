{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Restyler.ConfigSpec
  ( spec
  ) where

import Restyler.Prelude

import Data.Text.IO (hPutStr)
import OptEnvConf.Args (parseArgs)
import OptEnvConf.EnvMap qualified as EnvMap
import OptEnvConf.Run (runParserOn)
import Restyler.Config
import System.IO (hClose)
import Test.Hspec
import UnliftIO.Temporary (withSystemTempFile)

spec :: Spec
spec = do
  -- This test is a maintainence burden, in that when config/default.yaml
  -- changes, we need a corresponding update here. But it ensures we don't
  -- unintentionally break loading the defaults.
  it "uses defined defaults" $ do
    config <- loadTestConfig [] [] ["Foo.hs"]
    config.enabled `shouldBe` True
    config.exclude
      `shouldBe` [ "**/*.patch"
                 , "**/.git/**/*"
                 , "**/node_modules/**/*"
                 , "**/vendor/**/*"
                 , ".github/workflows/**/*"
                 ]
    config.commitTemplate `shouldBe` "Restyled by ${restyler.name}\n"
    config.remoteFiles `shouldBe` []
    config.ignores
      `shouldBe` Ignores
        { byAuthor = ["*[bot]"]
        , byBranch = ["renovate/*"]
        , byLabels = ["restyled-ignore"]
        }
    config.restylersVersion `shouldBe` "stable"
    config.restylerOverrides `shouldBe` [wildcard]

loadTestConfig
  :: HasCallStack
  => [Text]
  -- ^ Lines of Yaml
  --
  -- Empty means to behave as if no file at all.
  -> [(String, String)]
  -- ^ ENV
  -> [String]
  -- ^ Args
  -> IO Config
loadTestConfig yaml env args = do
  result <- case yaml of
    [] -> runParser ["config/default.yaml"]
    ls -> withSystemTempFile "restyler-test-config.yaml" $ \path h -> do
      hPutStr h (unlines ls) >> hClose h
      runParser [path, "config/default.yaml"]

  case result of
    Left errs -> do
      expectationFailure $ show errs
      error "unreachable"
    Right config -> pure config
 where
  runParser paths =
    runParserOn
      Nothing
      (configParser paths)
      (parseArgs args)
      (EnvMap.parse env)
      Nothing

wildcard :: RestylerOverride
wildcard = (restylerOverride "*") {enabled = Just True}
