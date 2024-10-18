{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Restyler.ConfigSpec
  ( spec
  ) where

import Restyler.Prelude

import Data.Text.IO (hPutStr)
import OptEnvConf.Args (parseArgs)
import OptEnvConf.EnvMap qualified as EnvMap
import OptEnvConf.Error (renderErrors)
import OptEnvConf.Run (runParserOn)
import Restyler.Config
import Restyler.Options
import Restyler.Options.Restrictions.Bytes
import System.IO (hClose)
import Test.Hspec
import Text.Colour.Capabilities (TerminalCapabilities (..))
import Text.Colour.Chunk (renderChunksText)
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
    -- config.options.logSettings `shouldBe` _
    config.options.dryRun `shouldBe` False
    config.options.failOnDifferences `shouldBe` False
    -- config.options.hostDirectory `shouldBe` _
    config.options.imageCleanup `shouldBe` False
    config.options.manifest `shouldBe` Nothing
    config.options.noCommit `shouldBe` False
    config.options.noClean `shouldBe` False
    config.options.noPull `shouldBe` False
    config.options.restrictions
      `shouldBe` Restrictions
        { netNone = True
        , cpuShares = Just 512
        , memory = Just $ Bytes {number = 128, suffix = Just M}
        }
    config.options.pullRequestJson `shouldBe` Nothing
    config.options.paths `shouldBe` pure "Foo.hs"

  it "can be overriden in user-configuration" $ do
    config <-
      loadTestConfig
        [ "enabled: false"
        , "exclude: [a, b]"
        , "also_exclude: [c, d]"
        , "commit_template: Hi"
        , "remote_files:"
        , "  - url: https://example.com"
        , "    path: example.txt"
        , "ignore:"
        , "  authors: [x]"
        , "  branches: ['y']"
        , "  labels: []"
        , "restylers_version: dev"
        , "restylers: [fourmolu]"
        , "cli:"
        , "  dry_run: true"
        , "  fail_on_differences: true"
        , "  image_cleanup: true"
        , "  clean: false"
        , "  commit: false"
        , "  pull: false"
        , "  restylers:"
        , "    net_none: false"
        , "    cpu_shares: 1024"
        , "    memory: { number: 512, suffix: k }"
        ]
        []
        ["Bar.hs"]

    config.enabled `shouldBe` False
    config.exclude `shouldBe` ["a", "b", "c", "d"]
    config.commitTemplate `shouldBe` "Hi"
    config.remoteFiles
      `shouldBe` [RemoteFile {url = "https://example.com", path = "example.txt"}]
    config.ignores
      `shouldBe` Ignores
        { byAuthor = ["x"]
        , byBranch = ["y"]
        , byLabels = []
        }
    config.restylersVersion `shouldBe` "dev"
    config.restylerOverrides `shouldBe` [enabled "fourmolu"]
    -- config.options.logSettings `shouldBe` _
    config.options.dryRun `shouldBe` True
    config.options.failOnDifferences `shouldBe` True
    -- config.options.hostDirectory `shouldBe` _
    config.options.imageCleanup `shouldBe` True
    config.options.manifest `shouldBe` Nothing
    config.options.noCommit `shouldBe` True
    config.options.noClean `shouldBe` True
    config.options.noPull `shouldBe` True
    config.options.restrictions
      `shouldBe` Restrictions
        { netNone = False
        , cpuShares = Just 1024
        , memory = Just $ Bytes {number = 512, suffix = Just K}
        }
    config.options.pullRequestJson `shouldBe` Nothing
    config.options.paths `shouldBe` pure "Bar.hs"

  context "legacy ignore options" $ do
    it "fully specified" $ do
      config <-
        loadTestConfig
          [ "ignore_authors: [a]"
          , "ignore_branches: [b]"
          , "ignore_labels: [c]"
          ]
          []
          ["Bar.hs"]

      config.ignores
        `shouldBe` Ignores
          { byAuthor = ["a"]
          , byBranch = ["b"]
          , byLabels = ["c"]
          }

    it "partially specified" $ do
      config <-
        loadTestConfig
          [ "ignore_authors: [a]"
          , "ignore_labels: [c]"
          ]
          []
          ["Bar.hs"]

      config.ignores
        `shouldBe` Ignores
          { byAuthor = ["a"]
          , byBranch = ["renovate/*"]
          , byLabels = ["c"]
          }

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
      expectationFailure
        $ unpack
        $ renderChunksText WithoutColours
        $ renderErrors errs
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
wildcard = enabled "*"

enabled :: Text -> RestylerOverride
enabled name = (restylerOverride name) {enabled = Just True}
