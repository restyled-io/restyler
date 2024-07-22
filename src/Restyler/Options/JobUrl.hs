module Restyler.Options.JobUrl
  ( JobUrl (..)
  , optJobUrl
  ) where

import Restyler.Prelude

import Options.Applicative

newtype JobUrl = JobUrl
  { unwrap :: URL
  }

optJobUrl :: Parser JobUrl
optJobUrl = JobUrl . URL <$> option str (long "job-url")
