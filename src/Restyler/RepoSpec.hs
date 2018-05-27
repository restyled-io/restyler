{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | String specification of a Repository's Pull Request
--
-- This shortened format is useful for passing a Pull Request as a command-line
-- argument, or showing it in log messages.
--
-- N.B. this should probably be called @PullRequestSpec@, or perhaps in-lined
-- into @"Restyler.PullRequest"@.
--
module Restyler.RepoSpec
    ( RepoSpec(..)
    , parseRepoSpec
    , showRepoSpec
    )
where

import Restyler.Prelude

import GitHub.Data
import qualified Prelude as Unsafe
import Text.Megaparsec
import Text.Megaparsec.Char

data RepoSpec = RepoSpec
    { rsOwner :: Name Owner
    , rsRepo :: Name Repo
    , rsPullRequest :: Int
    }
    deriving (Eq, Show)

-- | Parse @<owner>\/<name>#<number>@ into a @'RepoSpec'@
parseRepoSpec :: String -> Either String RepoSpec
parseRepoSpec = first parseErrorPretty . parse parser "<input>"

-- | Inverse of @'parseRepoSpec'@
showRepoSpec :: RepoSpec -> Text
showRepoSpec RepoSpec {..} =
    untagName rsOwner <> "/" <> untagName rsRepo <> "#" <> tshow rsPullRequest

type Parser = Parsec Void String

parser :: Parser RepoSpec
parser =
    RepoSpec
        <$> (mkName Proxy . pack <$> manyTill nonSpace (char '/'))
        <*> (mkName Proxy . pack <$> manyTill nonSpace (char '#'))
        <*> (Unsafe.read <$> some digitChar)

nonSpace :: Parser Char
nonSpace = satisfy $ not . isSpace
