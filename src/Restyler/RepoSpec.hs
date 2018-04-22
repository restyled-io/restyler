module Restyler.RepoSpec
    ( RepoSpec(..)
    , parseRepoSpec
    ) where

import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.Proxy
import qualified Data.Text as T
import Data.Void
import GitHub.Data
import Text.Megaparsec
import Text.Megaparsec.Char

data RepoSpec = RepoSpec
    { rsOwner :: Name Owner
    , rsRepo :: Name Repo
    , rsPullRequest :: Id PullRequest
    }
    deriving (Eq, Show)

-- | Parse @<owner>\/<name>#<number>@ into a @'RepoSpec'@
parseRepoSpec :: String -> Either String RepoSpec
parseRepoSpec = first show . parse parser "<input>"

type Parser = Parsec Void String

parser :: Parser RepoSpec
parser =
    RepoSpec
        <$> (mkName Proxy . T.pack <$> manyTill nonSpace (char '/'))
        <*> (mkName Proxy . T.pack <$> manyTill nonSpace (char '#'))
        <*> (mkId Proxy . read <$> some digitChar)

nonSpace :: Parser Char
nonSpace = satisfy $ not . isSpace
