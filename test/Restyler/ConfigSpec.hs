{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- |
--
-- Module      : Restyler.ConfigSpec
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.ConfigSpec
  ( spec
  ) where

import Restyler.Prelude

import Blammo.Logging.LogSettings (defaultLogSettings, shouldLogLevel)
import Data.Text.IO (hPutStr)
import Path (mkAbsFile)
import Restyler.Config
import Restyler.Config.Restrictions.Bytes
import Restyler.Test.Fixtures
import Restyler.Test.OptEnvConf
import System.IO (hClose)
import Test.Hspec
import UnliftIO.Temporary (withSystemTempFile)

spec :: Spec
spec = do
  context "options" $ do
    let checkOption name p = do
          it ("supports --" <> name) $ do
            config <- loadTestConfig ["--" <> name, "Foo.hs"] [] []
            p config `shouldBe` True

    checkOption "debug" $ \config ->
      let
        settings = modLogSettings config.logSettings defaultLogSettings
        logsLevel = shouldLogLevel settings ""
      in
        logsLevel LevelDebug && not (logsLevel $ LevelOther "trace")

    checkOption "fail-on-differences" (.failOnDifferences)

    checkOption "image-cleanup" (.imageCleanup)

    checkOption "no-commit" (.noCommit)

    checkOption "no-pull" (.noPull)

    it "supports --manifest" $ do
      config <- loadTestConfig ["--manifest", "/foo/bar/x.yaml", "Foo.hs"] [] []
      config.restylersManifest `shouldBe` Just $(mkAbsFile "/foo/bar/x.yaml")

  context "configuration" $ do
    -- This test is a maintainence burden, in that when config/default.yaml
    -- changes, we need a corresponding update here. But it ensures we don't
    -- unintentionally break loading the defaults.
    it "uses defined defaults" $ do
      config <- loadTestConfig ["Foo.hs"] [] []

      -- config.logSettings `shouldBe` _
      config.enabled `shouldBe` True
      config.exclude
        `shouldBe` [ "**/*.patch"
                   , "**/.git/**/*"
                   , "**/node_modules/**/*"
                   , "**/vendor/**/*"
                   , ".github/workflows/**/*"
                   ]
      config.dryRun `shouldBe` False
      config.failOnDifferences `shouldBe` False
      config.commitTemplate `shouldBe` "Restyled by ${restyler.name}\n"
      config.remoteFiles `shouldBe` []
      config.ignores
        `shouldBe` Ignores
          { byAuthor = ["*[bot]"]
          , byBranch = ["renovate/*"]
          , byLabels = ["restyled-ignore"]
          }
      config.restylersVersion `shouldBe` "stable"
      config.restylersManifest `shouldBe` Nothing
      config.restylerOverrides `shouldBe` [wildcard]
      -- config.hostDirectory `shouldBe` _
      config.imageCleanup `shouldBe` False
      config.noCommit `shouldBe` False
      config.noClean `shouldBe` False
      config.noPull `shouldBe` False
      config.restrictions
        `shouldBe` Restrictions
          { netNone = True
          , cpuShares = Just 512
          , memory = Just $ Bytes {number = 128, suffix = Just M}
          }
      config.pullRequestJson `shouldBe` Nothing
      config.paths `shouldBe` pure "Foo.hs"

    it "can be overriden in user-configuration" $ do
      config <-
        loadTestConfig
          ["Bar.hs"]
          []
          [ "enabled: false"
          , "exclude: [a, b]"
          , "dry_run: true"
          , "fail_on_differences: true"
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
          , "docker:"
          , "  image_cleanup: true"
          , "  pull: false"
          , "  restyler:"
          , "    net_none: false"
          , "    cpu_shares: 1024"
          , "    memory: 512k"
          , "git:"
          , "  clean: false"
          , "  commit: false"
          ]

      -- config.logSettings `shouldBe` _
      config.enabled `shouldBe` False
      config.exclude `shouldBe` ["a", "b", "c", "d"]
      config.dryRun `shouldBe` True
      config.failOnDifferences `shouldBe` True
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
      config.restylersManifest `shouldBe` Nothing
      config.restylerOverrides `shouldBe` [enabled "fourmolu"]
      -- config.hostDirectory `shouldBe` _
      config.imageCleanup `shouldBe` True
      config.noCommit `shouldBe` True
      config.noClean `shouldBe` True
      config.noPull `shouldBe` True
      config.restrictions
        `shouldBe` Restrictions
          { netNone = False
          , cpuShares = Just 1024
          , memory = Just $ Bytes {number = 512, suffix = Just K}
          }
      config.pullRequestJson `shouldBe` Nothing
      config.paths `shouldBe` pure "Bar.hs"

    context "remote_files" $ do
      it "supports URLs" $ do
        config <-
          loadTestConfig
            ["Bar.hs"]
            []
            [ "remote_files:"
            , "  - https://example.com/foo/bar.txt"
            ]

        config.remoteFiles
          `shouldBe` [ RemoteFile
                         { url = "https://example.com/foo/bar.txt"
                         , path = "bar.txt"
                         }
                     ]

      it "accepts path-less URLs if path is given" $ do
        config <-
          loadTestConfig
            ["Bar.hs"]
            []
            [ "remote_files:"
            , "  - url: https://example.com"
            , "    path: bar.txt"
            ]

        config.remoteFiles
          `shouldBe` [ RemoteFile
                         { url = "https://example.com"
                         , path = "bar.txt"
                         }
                     ]

      it "rejects path-less URLs if path is not given" $ do
        result <-
          loadTestConfigEither
            ["Bar.hs"]
            []
            [ "remote_files:"
            , "  - https://example.com/"
            ]

        void result `shouldSatisfy` isLeft

    context "restyler overrides" $ do
      it "supports names" $ do
        config <-
          loadTestConfig
            ["x.hs"]
            []
            [ "restylers:"
            , "  - fourmolu"
            , "  - \"!stylish-haskell\""
            ]

        config.restylerOverrides
          `shouldBe` [enabled "fourmolu", disabled "stylish-haskell"]

      it "supports name-keys" $ do
        config <-
          loadTestConfig
            ["x.hs"]
            []
            [ "restylers:"
            , "  - fourmolu:"
            , "      enabled: true"
            , "  - stylish-haskell:"
            , "      enabled: false"
            ]

        config.restylerOverrides
          `shouldBe` [enabled "fourmolu", disabled "stylish-haskell"]

      it "supports objects with names" $ do
        config <-
          loadTestConfig
            ["x.hs"]
            []
            [ "restylers:"
            , "  - name: fourmolu"
            , "    enabled: true"
            , "  - name: stylish-haskell"
            , "    enabled: false"
            ]

        config.restylerOverrides
          `shouldBe` [enabled "fourmolu", disabled "stylish-haskell"]

    context "legacy ignore options" $ do
      it "fully specified" $ do
        config <-
          loadTestConfig
            ["Bar.hs"]
            []
            [ "ignore_authors: [a]"
            , "ignore_branches: [b]"
            , "ignore_labels: [c]"
            ]

        config.ignores
          `shouldBe` Ignores
            { byAuthor = ["a"]
            , byBranch = ["b"]
            , byLabels = ["c"]
            }

      it "partially specified" $ do
        config <-
          loadTestConfig
            ["Bar.hs"]
            []
            [ "ignore_authors: [a]"
            , "ignore_labels: [c]"
            ]

        config.ignores
          `shouldBe` Ignores
            { byAuthor = ["a"]
            , byBranch = ["renovate/*"]
            , byLabels = ["c"]
            }

loadTestConfig
  :: HasCallStack
  => [String]
  -- ^ Options
  -> [(String, String)]
  -- ^ Environment
  -> [Text]
  -- ^ Configuration (lines of yaml)
  --
  -- Empty means to behave as if no file at all.
  -> IO Config
loadTestConfig args env yaml =
  withConfigYamls yaml $ \ps ->
    runParser (configParser ps) args env Nothing

loadTestConfigEither
  :: [String]
  -> [(String, String)]
  -> [Text]
  -> IO (Either (NonEmpty ParseError) Config)
loadTestConfigEither args env yaml =
  withConfigYamls yaml $ \ps ->
    runParserEither (configParser ps) args env Nothing

withConfigYamls :: [Text] -> ([FilePath] -> IO a) -> IO a
withConfigYamls yaml f = do
  case yaml of
    [] -> f []
    ls -> withSystemTempFile "restyler-test-config.yaml" $ \path h -> do
      hPutStr h (unlines ls) >> hClose h
      f [path]
