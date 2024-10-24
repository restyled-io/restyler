module Restyler.Docs
  ( DocsPage (..)
  , renderDocsPage
  )
where

import Restyler.Prelude

import Autodocodec.Yaml (jsonSchemaChunkLines)
import Data.Text qualified as T
import OptEnvConf.Args (Dashed (..))
import OptEnvConf.Doc
  ( AnyDocs (..)
  , ConfDoc (..)
  , EnvDoc (..)
  , OptDoc (..)
  , SetDoc (..)
  , docsToConfDocs
  , docsToEnvDocs
  , docsToOptDocs
  , parserDocs
  , withHelpAndVersionDocs
  , withoutHiddenDocs
  )
import OptEnvConf.Parser (Parser)
import Paths_restyler qualified as Pkg
import Restyler.Docs.Ronn
import Text.Colour (Chunk, chunkText, hPutChunksLocaleWith)
import Text.Colour.Capabilities.FromEnv (getTerminalCapabilitiesFromHandle)

data DocsPage = Restyle1 | RestyledYaml5

renderDocsPage :: DocsPage -> Parser a -> IO b
renderDocsPage docsPage p = do
  tc <- getTerminalCapabilitiesFromHandle stdout
  hPutChunksLocaleWith tc stdout $ formatDocsPage docsPage p
  exitSuccess

formatDocsPage :: DocsPage -> Parser a -> [Chunk]
formatDocsPage = \case
  Restyle1 -> runRonn ronnRestyle1
  RestyledYaml5 -> runRonn ronnRestyledYaml5

runRonn :: (AnyDocs (Maybe SetDoc) -> Ronn) -> Parser a -> [Chunk]
runRonn docsToRonn =
  formatRonn
    . docsToRonn
    . withHelpAndVersionDocs (Just Pkg.version)
    . parserDocs

ronnRestyle1 :: AnyDocs (Maybe SetDoc) -> Ronn
ronnRestyle1 docs =
  Ronn
    { name = "restyle"
    , number = 1
    , description = "Restyle local files"
    , sections =
        synopsis "restyle" optDocs
          <> options optDocs
          <> environment envDocs
          <> files fileDocs
          <> seeAlso refs
    }
 where
  optDocs :: [OptDoc]
  optDocs = mapAnyDocs id $ withoutHiddenDocs $ docsToOptDocs docs

  envDocs :: [EnvDoc]
  envDocs = mapAnyDocs id $ withoutHiddenDocs $ Just <$> docsToEnvDocs docs

  fileDocs :: [(FilePath, [RonnPart])]
  fileDocs =
    [
      ( ".restyled.yaml"
      , ["Configuration for Restyled, see ", RonnRef "restyled.yaml" 5]
      )
    ]

  refs :: [RonnPart]
  refs =
    [ RonnRef "restyled.yaml" 5
    , RonnRef "docker" 1
    , RonnRef "git" 1
    ]

ronnRestyledYaml5 :: AnyDocs (Maybe SetDoc) -> Ronn
ronnRestyledYaml5 docs =
  Ronn
    { name = "restyled.yaml"
    , number = 5
    , description = "Restyled configuration file"
    , sections = synopsis ".restyled.yaml" [] <> settings confDocs
    }
 where
  confDocs :: [ConfDoc]
  confDocs = mapAnyDocs id $ withoutHiddenDocs $ Just <$> docsToConfDocs docs

--------------------------------------------------------------------------------
-- Generic EnvOptConf -> Ronn helpers
--------------------------------------------------------------------------------

synopsis :: Text -> [OptDoc] -> [RonnGroup]
synopsis name = section "SYNOPSIS" . pure . RonnGroup . pure . synopsisLine
 where
  synopsisLine :: [OptDoc] -> RonnLine
  synopsisLine =
    RonnLine
      . (RonnBacktick (RonnRaw name) :)
      . map ronnOptionForSynopsis

options :: [OptDoc] -> [RonnGroup]
options =
  section "OPTIONS"
    . ronnDefList
      ronnOptionForOptions
      (pure . RonnLine . pure . ronnHelp . optDocHelp)

environment :: [EnvDoc] -> [RonnGroup]
environment =
  section "ENVIRONMENT"
    . ronnDefList
      ( \doc ->
          addRonnMeta (envDocMetavar doc)
            $ RonnConcat
            $ intersperse "|"
            $ map (RonnBacktick . RonnRaw . pack)
            $ toList
            $ envDocVars doc
      )
      (pure . RonnLine . pure . ronnHelp . envDocHelp)

settings :: [ConfDoc] -> [RonnGroup]
settings =
  section "DESCRIPTION"
    . ronnDefList
      ( \doc -> case getSchemaLines doc of
          SingleType x ->
            RonnConcat
              [ RonnBacktick
                  $ RonnRaw
                  $ pack
                  $ intercalate "."
                  $ concatMap (toList . fst)
                  $ toList
                  $ confDocKeys doc
              , "="
              , RonnRaw x
              ]
          ArrayType x ->
            RonnConcat
              [ RonnBacktick
                  $ RonnRaw
                  $ pack
                  $ intercalate "."
                  $ concatMap (toList . fst)
                  $ toList
                  $ confDocKeys doc
              , "="
              , RonnAngles $ RonnBracket $ RonnRaw x
              ]
          Unknown {} ->
            RonnConcat
              [ RonnBacktick
                  $ RonnRaw
                  $ pack
                  $ intercalate "."
                  $ concatMap (toList . fst)
                  $ toList
                  $ confDocKeys doc
              , "="
              , RonnAngles "schema"
              ]
      )
      ( \doc -> case getSchemaLines doc of
          SingleType {} ->
            [ RonnLine $ pure $ ronnHelp $ confDocHelp doc
            ]
          ArrayType {} ->
            [ RonnLine $ pure $ ronnHelp $ confDocHelp doc
            ]
          Unknown ls ->
            [ RonnLine $ pure $ ronnHelp $ confDocHelp doc
            , RonnLine []
            ]
              <> map (indentRonnLine 4) ls
      )

data SchemaLines
  = SingleType Text
  | ArrayType Text
  | Unknown [RonnLine]

getSchemaLines :: ConfDoc -> SchemaLines
getSchemaLines doc =
  case texts of
    [t]
      | Just x <- T.stripPrefix "- <" t
      , Just y <- T.stripSuffix ">" x ->
          ArrayType y
      | otherwise -> SingleType t
    ts -> Unknown $ map (RonnLine . pure . RonnRaw) ts
 where
  texts = filter (/= "# or null") $ map (mconcat . map chunkText) chunks
  chunks =
    concatMap (jsonSchemaChunkLines . snd)
      $ toList
      $ confDocKeys doc

files :: [(FilePath, [RonnPart])] -> [RonnGroup]
files =
  section "FILES"
    . ronnDefList
      (RonnBacktick . RonnRaw . pack . fst)
      (pure . RonnLine . pure . RonnConcat . snd)

seeAlso
  :: [RonnPart]
  -- ^ Assumed to be 'RonnRef's
  -> [RonnGroup]
seeAlso = section "SEE ALSO" . pure . RonnGroup . pure . refLine
 where
  refLine :: [RonnPart] -> RonnLine
  refLine rs = RonnLine [RonnConcat $ intersperse ", " rs]

section :: RonnPart -> [RonnGroup] -> [RonnGroup]
section name = (header :)
 where
  header = RonnGroup [RonnLine [RonnHeader name]]

ronnOptionForSynopsis :: OptDoc -> RonnPart
ronnOptionForSynopsis doc
  | null $ optDocDasheds doc, Just mv <- optDocMetavar doc = ronnMeta mv
  | otherwise = RonnBracket $ RonnConcat $ intersperse " \\| " $ ronnOptions doc

ronnOptionForOptions :: OptDoc -> RonnPart
ronnOptionForOptions doc
  | null $ optDocDasheds doc, Just mv <- optDocMetavar doc = ronnMeta mv
  | otherwise = RonnConcat $ intersperse ", " $ ronnOptions doc

ronnOptions :: OptDoc -> [RonnPart]
ronnOptions doc = map ronnShort shorts <> map ronnLong longs
 where
  (shorts, longs) = partitionDashed $ optDocDasheds doc

  ronnShort :: Char -> RonnPart
  ronnShort = ronnOption "-" . T.singleton

  ronnLong :: NonEmpty Char -> RonnPart
  ronnLong = addRonnMeta (optDocMetavar doc) . ronnOption "--" . pack . toList

  ronnOption :: Text -> Text -> RonnPart
  ronnOption prefix x = RonnBacktick $ RonnRaw $ prefix <> x

addRonnMeta :: Maybe String -> RonnPart -> RonnPart
addRonnMeta mMetavar x =
  maybe x (\mv -> RonnConcat [x, "=", ronnMeta mv]) mMetavar

ronnMeta :: String -> RonnPart
ronnMeta = RonnAngles . RonnRaw . pack

ronnHelp :: Maybe String -> RonnPart
ronnHelp = RonnRaw . pack . fromMaybe ""

ronnDefList
  :: (a -> RonnPart)
  -- ^ Get name
  -> (a -> [RonnLine])
  -- ^ Get defn
  -> [a]
  -> [RonnGroup]
ronnDefList toName toDefn = map $ \a -> ronnDefListItem (toName a) (toDefn a)

ronnDefListItem :: RonnPart -> [RonnLine] -> RonnGroup
ronnDefListItem name = RonnGroup . (nameLine :) . map (indentRonnLine 4)
 where
  nameLine = RonnLine ["  * " <> name <> ":"]

--------------------------------------------------------------------------------
-- Missing OptEnvConf helpers
--------------------------------------------------------------------------------

mapAnyDocs :: (a -> b) -> AnyDocs a -> [b]
mapAnyDocs f = \case
  AnyDocsCommands {} -> []
  AnyDocsAnd ds -> concatMap (mapAnyDocs f) ds
  AnyDocsOr ds -> concatMap (mapAnyDocs f) ds
  AnyDocsSingle vs -> [f vs]

partitionDashed :: [Dashed] -> ([Char], [NonEmpty Char])
partitionDashed = go ([], [])
 where
  go acc@(shorts, longs) = \case
    [] -> acc
    (DashedShort c : ds) -> go (shorts <> [c], longs) ds
    (DashedLong cs : ds) -> go (shorts, longs <> [cs]) ds
