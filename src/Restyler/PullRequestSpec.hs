{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | String specification of a Repository's Pull Request
--
-- This shortened format is useful for passing a Pull Request as a command-line
-- argument, or showing it in log messages.
--
module Restyler.PullRequestSpec
    ( PullRequestSpec(..)
    , parseSpec
    )
where

import Restyler.Prelude

import qualified Prelude as Unsafe
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char

data PullRequestSpec = PullRequestSpec
    { prsOwner :: Name Owner
    , prsRepo :: Name Repo
    , prsPullRequest :: Int
    }
    deriving Eq

instance Show PullRequestSpec where
    show PullRequestSpec {..} = unpack
        $ untagName prsOwner
        <> "/"
        <> untagName prsRepo
        <> "#"
        <> tshow prsPullRequest

-- | Parse @\<owner>\/\<name>#\<number>@ into a @'PullRequestSpec'@
parseSpec :: String -> Either String PullRequestSpec
parseSpec = first errorBundlePretty . parse parser "<input>"

-- | Inverse of @'parseSpec'@

type Parser = Parsec Void String

parser :: Parser PullRequestSpec
parser =
    PullRequestSpec
        <$> (mkName Proxy . pack <$> manyTill nonSpace (char '/'))
        <*> (mkName Proxy . pack <$> manyTill nonSpace (char '#'))
        <*> (Unsafe.read <$> some digitChar)

nonSpace :: Parser Char
nonSpace = satisfy $ not . isSpace
