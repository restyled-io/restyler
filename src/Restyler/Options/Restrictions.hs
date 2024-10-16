-- |
--
-- Module      : Restyler.Options.Restrictions
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Options.Restrictions
  ( HasOption

    -- * Unrestricted
  , Unrestricted
  , unrestrictedSpec

    -- * @--net=none@
  , RestylerNoNetNone
  , restylerNoNetNoneSpec

    -- * @--cpu-shares=<number>@
  , RestylerCpuShares
  , restylerCpuSharesSpec

    -- * @--memory=<number>[b|k|m|g]@
  , RestylerMemory
  , restylerMemorySpec

    -- * For use with @docker-run@
  , HasRestrictions
  , getRestrictionArgs

    -- * Bytes
  , Bytes (..)
  , Suffix (..)
  , bytesOption
  , readBytes
  ) where

import Restyler.Prelude

import Data.Aeson (FromJSON (..), withText)
import Data.Char qualified as Char
import Env qualified
import Options.Applicative qualified as Opt
import Restyler.Option

data Unrestricted

unrestrictedSpec :: OptionSpec Unrestricted Bool
unrestrictedSpec =
  OptionSpec
    { envParser = Env.flag Nothing (Just True) "UNRESTRICTED" $ Env.help help
    , optParser =
        Opt.flag Nothing (Just True)
          $ mconcat
            [ Opt.long "unrestricted"
            , Opt.help help
            ]
    }
 where
  help :: String
  help = "Do not restrict restyler resources"

data RestylerNoNetNone

restylerNoNetNoneSpec :: OptionSpec RestylerNoNetNone Bool
restylerNoNetNoneSpec =
  OptionSpec
    { envParser = Env.flag Nothing (Just True) "RESTYLER_NO_NET_NONE" $ Env.help help
    , optParser =
        Opt.flag Nothing (Just True)
          $ mconcat
            [ Opt.long "restyler-no-net-none"
            , Opt.help help
            ]
    }
 where
  help :: String
  help = "Run restylers without --net=none"

data RestylerCpuShares

restylerCpuSharesSpec :: OptionSpec RestylerCpuShares Natural
restylerCpuSharesSpec =
  OptionSpec
    { envParser =
        Env.var (fmap Just . Env.eitherReader readNat) "RESTYLER_CPU_SHARES"
          $ mconcat
            [ Env.help help
            , Env.def Nothing
            ]
    , optParser =
        Opt.option (Just <$> Opt.eitherReader readNat)
          $ mconcat
            [ Opt.long "restyler-cpu-shares"
            , Opt.metavar "NUMBER"
            , Opt.help help
            , Opt.value Nothing
            ]
    }
 where
  help :: String
  help = "Run restylers with --cpu-shares=<number>"

data RestylerMemory

restylerMemorySpec :: OptionSpec RestylerMemory Bytes
restylerMemorySpec =
  OptionSpec
    { envParser =
        Env.var (fmap Just . Env.eitherReader readBytes) "RESTYLER_MEMORY"
          $ mconcat
            [ Env.help help
            , Env.def Nothing
            ]
    , optParser =
        Opt.option (Just <$> Opt.eitherReader readBytes)
          $ mconcat
            [ Opt.long "restyler-memory"
            , Opt.metavar "NUMBER[b|k|m|g]"
            , Opt.help help
            , Opt.value Nothing
            ]
    }
 where
  help :: String
  help = "Run restylers with --memory=<number>[b|k|m|g]"

type HasRestrictions env =
  ( HasOption Unrestricted env Bool
  , HasOption RestylerNoNetNone env Bool
  , HasOption RestylerCpuShares env Natural
  , HasOption RestylerMemory env Bytes
  )

getRestrictionArgs
  :: (MonadReader env m, HasRestrictions env) => m [String]
getRestrictionArgs = do
  unrestricted <- lookupOptionDefault @Unrestricted False

  if unrestricted
    then pure []
    else do
      noNetNone <- lookupOptionDefault @RestylerNoNetNone False
      cpuShares <- lookupOptionDefault @RestylerCpuShares @_ @_ @Natural 128
      memory <-
        lookupOptionDefault @RestylerMemory
          $ Bytes
            { number = 512
            , suffix = Just M
            }

      pure
        $ concat
          [ ["--net=none" | not noNetNone]
          , ["--cpu-shares", show cpuShares]
          , ["--memory", bytesOption memory]
          ]

data Bytes = Bytes
  { number :: Natural
  , suffix :: Maybe Suffix
  }
  deriving stock (Eq, Show)

instance FromJSON Bytes where
  parseJSON = withText "Bytes" $ either fail pure . readBytes . unpack

data Suffix = B | K | M | G
  deriving stock (Eq, Show)

readSuffix :: String -> Either String Suffix
readSuffix = \case
  "b" -> Right B
  "k" -> Right K
  "m" -> Right M
  "g" -> Right G
  x -> Left $ "Invalid suffix " <> x <> ", must be one of b, k, m, or g"

showSuffix :: Suffix -> String
showSuffix = \case
  B -> "b"
  K -> "k"
  M -> "m"
  G -> "g"

bytesOption :: Bytes -> String
bytesOption b = show b.number <> maybe "" showSuffix b.suffix

readBytes :: String -> Either String Bytes
readBytes x =
  Bytes
    <$> readNat number
    <*> traverse
      readSuffix
      (guarded (not . null) suffix)
 where
  (number, suffix) = span ((||) <$> (== '-') <*> Char.isDigit) x

readNat :: String -> Either String Natural
readNat n = first (const $ "Not a valid natural number: " <> n) (readEither n)
