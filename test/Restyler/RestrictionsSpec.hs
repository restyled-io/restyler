module Restyler.RestrictionsSpec
  ( spec
  ) where

import Restyler.Prelude

import qualified Env
import Restyler.Restrictions
import Test.Hspec
import qualified Prelude

spec :: Spec
spec = do
  describe "restrictionOptions" $ do
    it "always adds --net=none and --cap-drop=all" $ do
      let opts =
            restrictionOptions $
              Restrictions
                { cpuShares = Last Nothing
                , memory = Last Nothing
                }

      opts `shouldContain` ["--net", "none"]
      opts `shouldContain` ["--cap-drop", "all"]

    it "always has default cpu-shares and memory" $ do
      let opts = restrictionOptions fullRestrictions

      opts `shouldContain` ["--cpu-shares", "128"]
      opts `shouldContain` ["--memory", "512m"]

    it "respects cpu-shares and memory" $ do
      let opts =
            restrictionOptions $
              Restrictions
                { cpuShares = Last $ Just 256
                , memory = Last $ Just $ Bytes 1 $ Just G
                }

      opts `shouldContain` ["--cpu-shares", "256"]
      opts `shouldContain` ["--memory", "1g"]

  describe "envRestrictions" $ do
    it "uses full restrictions by default" $ do
      Env.parsePure envRestrictions [] `shouldBe` Right fullRestrictions

    it "treats empty UNRESTRICTED as full restrictions" $ do
      Env.parsePure envRestrictions [("UNRESTRICTED", "")]
        `shouldBe` Right fullRestrictions

    -- Env.switch means any non-empty value, even false-looking ones
    for_ (Prelude.words "x 0 1 on off true false yes no") $ \x -> do
      it ("treats UNRESTRICTED=" <> x <> " as no restrictions") $ do
        Env.parsePure envRestrictions [("UNRESTRICTED", x)]
          `shouldBe` Right
            Restrictions
              { cpuShares = Last Nothing
              , memory = Last Nothing
              }

    it "can adjust individual restrictions" $ do
      let
        env :: [(String, String)]
        env =
          [ ("RESTYLER_CPU_SHARES", "256")
          , ("RESTYLER_MEMORY", "1024m")
          ]

      Env.parsePure envRestrictions env
        `shouldBe` Right
          Restrictions
            { cpuShares = Last $ Just 256
            , memory = Last $ Just $ Bytes 1024 $ Just M
            }

    it "can adjust individual restrictions after UNRESTRICTED" $ do
      let
        env :: [(String, String)]
        env = [("UNRESTRICTED", "x"), ("RESTYLER_MEMORY", "256b")]

      Env.parsePure envRestrictions env
        `shouldBe` Right
          Restrictions
            { cpuShares = Last Nothing
            , memory = Last $ Just $ Bytes 256 $ Just B
            }

    it "accepts memory without suffix" $ do
      Env.parsePure envRestrictions [("RESTYLER_MEMORY", "1024")]
        `shouldBe` Right
          fullRestrictions
            { memory = Last $ Just $ Bytes 1024 Nothing
            }

    context "invalid input" $ do
      it "rejects invalid RESTYLER_CPU_SHARES" $ do
        let
          env :: [(String, String)]
          env = [("RESTYLER_CPU_SHARES", "-100")]

          msg :: String
          msg = "Not a valid natural number: -100"

        Env.parsePure envRestrictions env
          `shouldBe` Left
            [
              ( "RESTYLER_CPU_SHARES"
              , Env.UnreadError msg
              )
            ]

      it "rejects MEMORY for invalid number" $ do
        let
          env :: [(String, String)]
          env = [("RESTYLER_MEMORY", "-100b")]

          msg :: String
          msg = "Not a valid natural number: -100"

        Env.parsePure envRestrictions env
          `shouldBe` Left [("RESTYLER_MEMORY", Env.UnreadError msg)]

      it "rejects MEMORY for invalid suffix" $ do
        let
          env :: [(String, String)]
          env = [("RESTYLER_MEMORY", "100x")]

          msg :: String
          msg = "Invalid suffix x, must be one of b, k, m, or g"

        Env.parsePure envRestrictions env
          `shouldBe` Left [("RESTYLER_MEMORY", Env.UnreadError msg)]

      -- Not desired per se, but we'll cover it as documentation
      it
        "rejects RESTYLER_MEMORY for invalid number before checking suffix"
        $ do
          let
            env :: [(String, String)]
            env = [("RESTYLER_MEMORY", "-100x")]

            msg :: String
            msg = "Not a valid natural number: -100"

          Env.parsePure envRestrictions env
            `shouldBe` Left
              [
                ( "RESTYLER_MEMORY"
                , Env.UnreadError msg
                )
              ]
