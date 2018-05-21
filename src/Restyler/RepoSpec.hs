module Restyler.RepoSpec
    ( RepoSpec(..)
    , parseRepoSpec
    ) where

import Restyler.Prelude

import Data.Bifunctor (first)
import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Void
import GitHub.Data
import qualified Prelude as Unsafe
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
parseRepoSpec = first parseErrorPretty . parse parser "<input>"

type Parser = Parsec Void String

parser :: Parser RepoSpec
parser =
    RepoSpec
        <$> (mkName Proxy . T.pack <$> manyTill nonSpace (char '/'))
        <*> (mkName Proxy . T.pack <$> manyTill nonSpace (char '#'))
        <*> (mkId Proxy . Unsafe.read <$> some digitChar)

nonSpace :: Parser Char
nonSpace = satisfy $ not . isSpace
