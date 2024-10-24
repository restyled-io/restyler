module Restyler.Docs.Ronn
  ( Ronn (..)
  , RonnGroup (..)
  , RonnLine (..)
  , indentRonnLine
  , RonnPart (..)
  , ronnPartText
  , formatRonn
  ) where

import Restyler.Prelude

import Data.Text qualified as T
import Text.Colour (Chunk, chunk, unlinesChunks)

data Ronn = Ronn
  { name :: Text
  , number :: Int
  , description :: Text
  , sections :: [RonnGroup]
  }

formatRonn :: Ronn -> [Chunk]
formatRonn ronn =
  unlinesChunks
    $ concatMap formatRonnGroup
    $ intersperse (RonnGroup [RonnLine [""]])
    $ RonnGroup
      [ nameLine
      , RonnLine [RonnRaw $ T.replicate (ronnLineLength nameLine) "="]
      ]
    : ronn.sections
 where
  nameLine =
    RonnLine
      [ RonnConcat [RonnRaw ronn.name, RonnParen $ show ronn.number]
      , "--"
      , RonnRaw ronn.description
      ]

newtype RonnGroup = RonnGroup
  { unwrap :: [RonnLine]
  }

formatRonnGroup :: RonnGroup -> [[Chunk]]
formatRonnGroup = map ronnLineToChunks . (.unwrap)

newtype RonnLine = RonnLine
  { unwrap :: [RonnPart]
  }

ronnLineLength :: RonnLine -> Int
ronnLineLength = sum . map ronnPartLength . (.unwrap)

-- | Prepends the given number of spaces to the first 'RonnPart' of the line
indentRonnLine :: Int -> RonnLine -> RonnLine
indentRonnLine n = \case
  RonnLine [] -> RonnLine []
  RonnLine (p : ps) -> RonnLine $ (spaces <> p) : ps
 where
  spaces = RonnRaw $ pack $ replicate n ' '

ronnLineToChunks :: RonnLine -> [Chunk]
ronnLineToChunks = map (chunk . ronnPartText) . intersperse " " . (.unwrap)

data RonnPart
  = RonnConcat [RonnPart]
  | RonnHeader RonnPart
  | RonnBacktick RonnPart
  | RonnBracket RonnPart
  | RonnAngles RonnPart
  | RonnParen RonnPart
  | RonnRef Text Int
  | RonnRaw Text

instance IsString RonnPart where
  fromString = RonnRaw . pack

instance Semigroup RonnPart where
  RonnConcat as <> RonnConcat bs = RonnConcat $ as <> bs
  RonnConcat as <> b = RonnConcat $ as <> [b]
  a <> RonnConcat bs = RonnConcat $ a : bs
  a <> b = RonnConcat [a, b]

instance Monoid RonnPart where
  mempty = RonnConcat []

ronnPartText :: RonnPart -> Text
ronnPartText = \case
  RonnConcat xs -> mconcat $ map ronnPartText xs
  RonnHeader x -> "## " <> ronnPartText x
  RonnBacktick x -> "`" <> ronnPartText x <> "`"
  RonnBracket x -> "[" <> ronnPartText x <> "]"
  RonnAngles x -> "<" <> ronnPartText x <> ">"
  RonnParen x -> "(" <> ronnPartText x <> ")"
  RonnRef x i -> "**" <> x <> "(" <> show i <> ")**"
  RonnRaw x -> x

ronnPartLength :: RonnPart -> Int
ronnPartLength = T.length . ronnPartText
