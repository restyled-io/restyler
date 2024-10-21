-- |
--
-- Module      : Restyler.RestrictionsSpec
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.RestrictionsSpec
  ( spec
  ) where

import Restyler.Prelude

import Restyler.Config.Restrictions
import Restyler.Config.Restrictions.Bytes
import Restyler.Test.OptEnvConf
import Test.Hspec

spec :: Spec
spec = do
  describe "restrictionOptions" $ do
    it "is the correct docker-run flags" $ do
      let
        noRestrictions =
          Restrictions
            { netNone = False
            , cpuShares = Nothing
            , memory = Nothing
            }

        fullRestrictions =
          Restrictions
            { netNone = True
            , cpuShares = Just 128
            , memory = Just $ Bytes 512 $ Just M
            }

      restrictionOptions noRestrictions `shouldBe` []
      restrictionOptions fullRestrictions
        `shouldBe` [ "--net=none"
                   , "--cpu-shares=128"
                   , "--memory=512m"
                   ]

  describe "restrictionsParser" $ do
    it "uses full restrictions by default" $ do
      rs <- parseEnv restrictionsParser []
      rs.netNone `shouldBe` True
      rs.cpuShares `shouldSatisfy` isJust
      rs.memory `shouldSatisfy` isJust

    it "accepts RESTRICTED=False" $ do
      rs <- parseEnv restrictionsParser [("RESTRICTED", "False")]
      rs.netNone `shouldBe` False
      rs.cpuShares `shouldBe` Nothing
      rs.memory `shouldBe` Nothing

    it "can adjust individual restrictions" $ do
      let
        env :: [(String, String)]
        env =
          [ ("CPU_SHARES", "256")
          , ("MEMORY", "1024m")
          ]

      rs <- parseEnv restrictionsParser env
      rs.cpuShares `shouldBe` Just 256
      rs.memory `shouldBe` Just (Bytes 1024 $ Just M)

    it "accepts memory without suffix" $ do
      rs <- parseEnv restrictionsParser [("MEMORY", "1024")]
      rs.memory `shouldBe` Just (Bytes 1024 Nothing)

    context "invalid input" $ do
      it "rejects invalid CPU_SHARES" $ do
        rs <- parseEnvEither restrictionsParser [("CPU_SHARES", "-100")]
        rs `shouldHaveParseError` readError "Not a valid natural number: -100"

      it "rejects MEMORY for invalid number" $ do
        rs <- parseEnvEither restrictionsParser [("MEMORY", "-100b")]
        rs `shouldHaveParseError` readError "Not a valid natural number: -100"

      it "rejects MEMORY for invalid suffix" $ do
        rs <- parseEnvEither restrictionsParser [("MEMORY", "100x")]
        rs
          `shouldHaveParseError` readError "Invalid suffix x, must be one of b, k, m, or g"

      -- Not desired per se, but we'll cover it as documentation
      it "rejects MEMORY for invalid number before suffix" $ do
        rs <- parseEnvEither restrictionsParser [("MEMORY", "-100x")]
        rs `shouldHaveParseError` readError "Not a valid natural number: -100"

readError :: String -> ParseErrorMessage -> Bool
readError needle = \case
  ParseErrorArgumentRead _ es -> needle `elem` es
  ParseErrorOptionRead _ es -> needle `elem` es
  ParseErrorEnvRead _ es -> needle `elem` es
  ParseErrorConfigRead _ e -> e == needle
  _ -> False

parseEnv :: HasCallStack => Parser a -> [(String, String)] -> IO a
parseEnv p env = runParser p [] env $ Just defaults

parseEnvEither
  :: Parser a -> [(String, String)] -> IO (Either (NonEmpty ParseError) a)
parseEnvEither p env = runParserEither p [] env $ Just defaults

defaults :: Object
defaults =
  mkObject
    [ "restricted" .= True
    , "net_none" .= True
    , "cpu_shares" .= (512 :: Natural)
    , "memory" .= ("128m" :: Text)
    ]
