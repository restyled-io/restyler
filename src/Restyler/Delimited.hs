{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

-- |
--
-- Module      : Restyler.Delimited
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.Delimited
  ( Delimiters (..)
  , restyleDelimited

    -- * Exported for testing
  , DelimitedPath (..)
  , DelimitedPathPart (..)
  , DelimitedMeta (..)
  , delimit
  , undelimit
  ) where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Casing
import Data.Text qualified as T
import Restyler.Config.ExpectedKeys
import Restyler.Monad.Directory
import Restyler.Monad.ReadFile
import Restyler.Monad.WriteFile
import UnliftIO.Exception (bracket)

data Delimiters = Delimiters
  { dStart :: Text
  , dEnd :: Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON Delimiters where
  parseJSON = genericParseJSONValidated $ aesonPrefix snakeCase

instance ToJSON Delimiters where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

data DelimitedPath = DelimitedPath
  { dpSource :: FilePath
  , dpParts :: [DelimitedPathPart]
  }
  deriving stock (Eq, Show)

data DelimitedPathPart = DelimitedPathPart
  { dppIn :: Bool
  , dppPath :: FilePath
  , dppMeta :: Maybe DelimitedMeta
  }
  deriving stock (Eq, Show)

data DelimitedMeta = DelimitedMeta
  { dmLeading :: Text
  , dmTrailing :: Text
  , dmIndent :: Natural
  }
  deriving stock (Eq, Show)

-- | Restyle delimited content within paths using the given function
restyleDelimited
  :: ( MonadUnliftIO m
     , MonadDirectory m
     , MonadReadFile m
     , MonadWriteFile m
     )
  => Delimiters
  -> ([FilePath] -> m result)
  -- ^ Restyle files inplace
  -> [FilePath]
  -> m result
restyleDelimited delimiters restyle paths =
  bracket
    (traverse (delimit delimiters) paths)
    (traverse (traverse (removeFile . dppPath) . dpParts))
    $ \delimited -> do
      result <- restyle $ concatMap delimitedInPaths delimited
      result <$ traverse_ (undelimit delimiters) delimited

delimitedInPaths :: DelimitedPath -> [FilePath]
delimitedInPaths = map dppPath . filter dppIn . dpParts

-- | Split a File into separate files of the content between delimiters
--
-- Given a @foo.rb@ containing text with delimiters, create
--
-- - @foo.rb.0@
-- - @foo.rb.1@
-- - @foo.rb.2@
-- - @foo.rb.n@
--
-- Where each file contains the content before, within, and after the given
-- delimiters (repeatedly). The returned value tracks which paths hold content
-- that was delimited /in/ or /out/.
delimit
  :: (MonadReadFile m, MonadWriteFile m) => Delimiters -> FilePath -> m DelimitedPath
delimit Delimiters {..} path = do
  content <- readFile path
  parts <-
    traverse (uncurry $ writePart path)
      $ zip [0 ..]
      $ splitBetween
        dStart
        dEnd
        content
  pure DelimitedPath {dpSource = path, dpParts = parts}

writePart
  :: MonadWriteFile m
  => FilePath
  -> Int
  -> Either Text Text
  -> m DelimitedPathPart
writePart path n part = do
  let path' = path <> "." <> show @String @Int n

  case part of
    Left content -> do
      writeFile path' content
      pure
        DelimitedPathPart
          { dppIn = False
          , dppPath = path'
          , dppMeta = Nothing
          }
    Right content -> do
      let (meta, content') = cleanDelimitedPart content
      writeFile path' content'
      pure
        DelimitedPathPart
          { dppIn = True
          , dppPath = path'
          , dppMeta = Just meta
          }

cleanDelimitedPart :: Text -> (DelimitedMeta, Text)
cleanDelimitedPart x =
  let
    leadingS = T.takeWhile (== ' ') x
    leadingN = T.takeWhile (== '\n') $ T.drop (T.length leadingS) x
    leading = leadingS <> leadingN
    trailing = T.takeWhileEnd (== ' ') x

    inner = T.dropEnd (T.length trailing) $ T.drop (T.length leading) x
    (indent, dedented) = dedent inner
  in
    ( DelimitedMeta
        { dmLeading = leading
        , dmTrailing = trailing
        , dmIndent = indent
        }
    , dedented
    )

-- | Re-construct a file from its delimited parts
undelimit
  :: (MonadReadFile m, MonadWriteFile m) => Delimiters -> DelimitedPath -> m ()
undelimit delimiters DelimitedPath {..} = do
  contents <- traverse (readPart delimiters) dpParts
  writeFile dpSource $ mconcat contents

readPart :: MonadReadFile m => Delimiters -> DelimitedPathPart -> m Text
readPart Delimiters {..} DelimitedPathPart {..}
  | not dppIn = readFile dppPath
  | otherwise = do
      content <- readFile dppPath
      pure
        $ mconcat
          [ dStart
          , maybe "" dmLeading dppMeta
          , maybe content (indented content . dmIndent) dppMeta
          , maybe "" dmTrailing dppMeta
          , dEnd
          ]

dedent :: Text -> (Natural, Text)
dedent t =
  ( fromIntegral minIndent
  , T.unlines $ map (T.drop minIndent . snd) lns
  )
 where
  lns = map (T.length . T.takeWhile isSpace &&& id) $ T.lines t

  -- For purposes of choosing min-indent, entirely blank lines are ignored
  minIndent =
    fromMaybe 0
      $ minimumMaybe
      $ map fst
      $ filter (not . T.null . snd) lns

indented :: Text -> Natural -> Text
indented content level = T.unlines $ map prefix $ T.lines content
 where
  -- For the purposes of indenting, we leave totally-blank lines alone
  prefix x
    | T.null x = x
    | otherwise = T.replicate (fromIntegral level) " " <> x

splitBetween :: Text -> Text -> Text -> [Either Text Text]
splitBetween d1 d2 = zig
 where
  zig x = case T.splitOn d1 x of
    [] -> []
    (p : ps) -> Left p : concatMap zag ps

  zag x = case T.splitOn d2 x of
    [] -> []
    (p : ps) -> Right p : map Left ps
