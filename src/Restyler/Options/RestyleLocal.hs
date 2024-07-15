module Restyler.Options.RestyleLocal
  ( Options (..)
  , getOptions
  ) where

import Restyler.Prelude

import Data.Semigroup.Generic
import Env qualified
import Options.Applicative
import Restyler.HostDirectoryOption
import Restyler.LogSettingsOption
import Restyler.Restrictions

data Options = Options
  { logSettings :: LogSettingsOption
  , restrictions :: Restrictions
  , hostDirectory :: HostDirectoryOption
  }
  deriving stock (Generic)
  deriving (Semigroup) via (GenericSemigroupMonoid Options)

instance HasRestrictions Options where
  restrictionsL = lens (.restrictions) $ \x y -> x {restrictions = y}

instance HasHostDirectoryOption Options where
  hostDirectoryOptionL = lens (.hostDirectory) $ \x y -> x {hostDirectory = y}

getOptions :: IO Options
getOptions =
  (<>)
    <$> envOptions
    <*> optOptions

envOptions :: IO Options
envOptions =
  Env.parse id
    $ Options
    <$> envLogSettingsOption
    <*> envRestrictions
    <*> envHostDirectoryOption

optOptions :: IO Options
optOptions = execParser $ info (p <**> helper) fullDesc
 where
  p =
    Options
      <$> optLogSettingsOption
      <*> optRestrictions
      <*> optHostDirectoryOption
