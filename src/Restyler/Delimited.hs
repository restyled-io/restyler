{-# LANGUAGE TypeApplications #-}

module Restyler.Delimited
    ( Delimiters(..)
    , restyleDelimited

    -- * Exported for testing
    , DelimitedPath(..)
    , DelimitedPathPart(..)
    , DelimitedMeta(..)
    , delimit
    , undelimit
    )
where

import Restyler.Prelude

import Data.Aeson
import Data.Aeson.Casing
import qualified Data.Text as T
import Restyler.App.Class
import Restyler.Config.ExpectedKeys

data Delimiters = Delimiters
    { dStart :: Text
    , dEnd :: Text
    }
    deriving (Eq, Show, Generic)

instance FromJSON Delimiters where
    parseJSON = genericParseJSONValidated $ aesonPrefix snakeCase

instance ToJSON Delimiters where
    toJSON = genericToJSON $ aesonPrefix snakeCase
    toEncoding = genericToEncoding $ aesonPrefix snakeCase

data DelimitedPath = DelimitedPath
    { dpSource :: FilePath
    , dpParts :: [DelimitedPathPart]
    }
    deriving (Eq, Show)

data DelimitedPathPart = DelimitedPathPart
    { dppIn :: Bool
    , dppPath :: FilePath
    , dppMeta :: Maybe DelimitedMeta
    }
    deriving (Eq, Show)

data DelimitedMeta = DelimitedMeta
    { dmLeading :: Text
    , dmTrailing :: Text
    , dmIndent :: Natural
    }
    deriving (Eq, Show)

-- | Restyle delimited content within paths using the given function
restyleDelimited
    :: HasSystem env
    => Delimiters
    -> ([FilePath] -> RIO env result) -- ^ Restyle files inplace
    -> [FilePath]
    -> RIO env result
restyleDelimited delimiters restyle paths = do
    delimited <- traverse (delimit delimiters) paths
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
--
delimit :: HasSystem env => Delimiters -> FilePath -> RIO env DelimitedPath
delimit Delimiters {..} path = do
    content <- readFile path
    parts <- traverse (uncurry $ writePart path) $ zip [0 ..] $ splitBetween
        dStart
        dEnd
        content
    pure DelimitedPath { dpSource = path, dpParts = parts }

writePart
    :: HasSystem env
    => FilePath
    -> Int
    -> Either Text Text
    -> RIO env DelimitedPathPart
writePart path n part = do
    let path' = path <> "." <> show @Int n

    case part of
        Left content -> do
            writeFile path' content
            pure DelimitedPathPart
                { dppIn = False
                , dppPath = path'
                , dppMeta = Nothing
                }
        Right content -> do
            let (meta, content') = cleanDelimitedPart content
            writeFile path' content'
            pure DelimitedPathPart
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
            , dmIndent = fromIntegral indent
            }
        , dedented
        )
  where
    dedent t =
        let
            lns = T.lines t
            lns' = map (T.length . T.takeWhile isSpace &&& id) lns
            minIndent = fromMaybe 0 $ minimumMaybe $ map fst lns'
        in (minIndent, T.unlines $ map (T.drop minIndent . snd) lns')

-- | Re-construct a file from its delimited parts
undelimit :: HasSystem env => Delimiters -> DelimitedPath -> RIO env ()
undelimit delimiters DelimitedPath {..} = do
    contents <- traverse (readPart delimiters) dpParts
    writeFile dpSource $ mconcat contents

readPart :: HasSystem env => Delimiters -> DelimitedPathPart -> RIO env Text
readPart Delimiters {..} DelimitedPathPart {..}
    | not dppIn = readFile dppPath
    | otherwise = do
        content <- readFile dppPath
        pure $ mconcat
            [ dStart
            , maybe "" dmLeading dppMeta
            , maybe content (indented content . dmIndent) dppMeta
            , maybe "" dmTrailing dppMeta
            , dEnd
            ]
  where
    indented content level =
        let prefix = T.replicate (fromIntegral level) " "
        in T.unlines $ map (prefix <>) $ T.lines content

splitBetween :: Text -> Text -> Text -> [Either Text Text]
splitBetween d1 d2 = zig
  where
    zig x = case T.splitOn d1 x of
        [] -> []
        (p : ps) -> Left p : concatMap zag ps

    zag x = case T.splitOn d2 x of
        [] -> []
        (p : ps) -> Right p : map Left ps
