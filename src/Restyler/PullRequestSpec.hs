-- | String specification of a Repository's Pull Request
--
-- This shortened format is useful for passing a Pull Request as a command-line
-- argument, or showing it in log messages.
--
module Restyler.PullRequestSpec
    ( PullRequestSpec(..)
    , pullRequestSpecToText
    , parseSpec
    ) where

import Restyler.Prelude

import GitHub.Data
import qualified Prelude as Unsafe
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char

data PullRequestSpec = PullRequestSpec
    { prsOwner :: Name Owner
    , prsRepo :: Name Repo
    , prsPullRequest :: IssueNumber
    }
    deriving stock (Eq, Show)

pullRequestSpecToText :: PullRequestSpec -> Text
pullRequestSpecToText PullRequestSpec {..} = mconcat
    [ untagName prsOwner <> "/"
    , untagName prsRepo <> "#"
    , toPathPart prsPullRequest
    ]

-- | Parse @\<owner>\/\<name>#\<number>@ into a @'PullRequestSpec'@
parseSpec :: String -> Either String PullRequestSpec
parseSpec = first errorBundlePretty . parse parser "<input>"

type Parser = Parsec Void String

parser :: Parser PullRequestSpec
parser =
    PullRequestSpec
        <$> (mkName Proxy . pack <$> manyTill nonSpace (char '/'))
        <*> (mkName Proxy . pack <$> manyTill nonSpace (char '#'))
        <*> (IssueNumber . Unsafe.read <$> some digitChar)

nonSpace :: Parser Char
nonSpace = satisfy $ not . isSpace
