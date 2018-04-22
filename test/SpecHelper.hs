{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SpecHelper
    ( module SpecHelper
    , module X
    )
    where

import ClassyPrelude as X hiding (dropEnd)

import Data.Char (isSpace)
import Data.Proxy
import qualified Data.Text as T
import GitHub.Data (Id, mkId)
import qualified Prelude as P
import Restyler.Process as X (callProcess)
import System.Directory (removeFile, setCurrentDirectory)
import System.IO.Temp as X (emptySystemTempFile, withSystemTempDirectory)
import System.Process as X (readProcess)
import Test.Hspec as X
import Text.Shakespeare.Text as X (st)

instance Num (Id a) where
    -- Just so we can type literals for Ids in Specs
    fromInteger = mkId Proxy . fromInteger

    (+) = error "NO"
    (-) = error "NO"
    (*) = error "NO"
    abs = error "NO"
    signum = error "NO"

setupGitRepo :: FilePath -> IO ()
setupGitRepo dir = do
    setCurrentDirectory dir
    callProcess "git" ["init"]
    callProcess "git" ["commit", "--allow-empty", "--message", "Test"]

-- | Dedent content
--
-- N.B. assumes an initial "\n" and trailing quote-end, which is what happens
-- when you use the st quasi-quoter in the following style:
--
-- > [st|
-- >   some code
-- >   some code
-- >   |]
--
-- In other words, do not use this on (e.g.) @\"Some text\n\"@
--
dedent :: Text -> Text
dedent t = T.unlines $ map (T.drop $ indent lns) lns
  where
    lns = dropEnd $ T.lines $ T.drop 1 t
    dropEnd = reverse . drop 1 . reverse

    indent [] = 0
    indent ts = P.minimum $ map (T.length . T.takeWhile isSpace) ts

withEmptySystemTempFile :: (FilePath -> IO a) -> IO a
withEmptySystemTempFile = bracket (emptySystemTempFile "") removeFile

hasError :: String -> Either String b -> Bool
hasError msg (Left err) = msg `isInfixOf` err
hasError _ _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
