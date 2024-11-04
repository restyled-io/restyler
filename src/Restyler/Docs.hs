{-# LANGUAGE RecordWildCards #-}

module Restyler.Docs
  ( DocsPage (..)
  , renderDocsPage
  )
where

import Restyler.Prelude

import Autodocodec.Yaml (jsonSchemaChunkLines)
import Data.Text.IO qualified as T
import OptEnvConf.Doc (AnyDocs (..), ConfDoc (..), commandDocs, parserConfDocs)
import OptEnvConf.Parser (Parser)
import Restyler.Config (configParser, configPaths)
import Ronn
import Ronn.OptEnvConf ()
import Text.Colour.Chunk (Chunk (..))

data DocsPage = Restyle1 | RestyledYaml5

renderDocsPage :: DocsPage -> IO b
renderDocsPage docsPage = do
  T.putStrLn $ formatDocsPage docsPage $ configParser []
  exitSuccess

formatDocsPage :: DocsPage -> Parser a -> Text
formatDocsPage = \case
  Restyle1 -> ronnToText . ronnRestyle1
  RestyledYaml5 -> ronnToText . ronnRestyledYaml5

ronnRestyle1 :: Parser a -> Ronn
ronnRestyle1 p =
  Ronn
    { name = ManRef "restyle" ManSection1
    , description = ["restyle local files"]
    , sections =
        getSections "restyle" p
          <> [ definitionsSection
                "FILES"
                [ Definition
                    { name = Code ".restyled.yaml"
                    , description =
                        Line
                          [ "Configuration for Restyled, see "
                          , Ref $ ManRef "restyled.yaml" ManSection5
                          ]
                    , content = Nothing
                    }
                ]
             , seeAlsoSection
                [ ManRef "restyled.yaml" ManSection5
                , ManRef "docker" ManSection1
                , ManRef "git" ManSection1
                ]
             ]
    }

ronnRestyledYaml5 :: Parser a -> Ronn
ronnRestyledYaml5 p =
  Ronn
    { name = ManRef "restyled.yaml" ManSection5
    , description = ["restyled configuration file"]
    , sections =
        [ Section
            { name = "SYNOPSIS"
            , content =
                [ Groups
                    [ Lines ["Restyled configuration, loaded from the first of:"]
                    , Lines $ map (Line . ("*" :) . pure . Code . fromString) configPaths
                    ]
                ]
            }
        , Section
            { name = "DESCRIPTION"
            , content = [Definitions $ renderConfDocs $ parserConfDocs p]
            }
        ]
    }

renderConfDocs :: AnyDocs ConfDoc -> [Definition]
renderConfDocs = \case
  AnyDocsCommands _ cs -> concatMap (renderConfDocs . commandDocs) cs
  AnyDocsAnd ds -> concatMap renderConfDocs ds
  AnyDocsOr ds -> concatMap renderConfDocs ds
  AnyDocsSingle ed -> renderConfDoc ed

renderConfDoc :: ConfDoc -> [Definition]
renderConfDoc ConfDoc {..} =
  toList $ uncurry toDefinition <$> confDocKeys
 where
  -- Different confs with different schema is not really supported. If we do
  -- that it'll generate separate definitions with the same help and default.
  (description, rest) = case lines . pack <$> confDocHelp of
    Nothing -> ("No help defined", Nothing)
    Just [] -> ("No help defined", Nothing)
    Just [x] -> (Line $ pure $ Raw x, Nothing)
    Just (x : xs) ->
      ( Line $ pure $ Raw x
      , Just $ Lines $ map (Line . pure . Raw) xs
      )

  toDefinition key schema =
    Definition
      { name = Code $ Concat $ intersperse "." $ toList $ fromString <$> key
      , description
      , content =
          Just
            [ Groups
                $ catMaybes
                  [ rest
                  , Just
                      $ Lines
                      $ ("Schema:" :)
                      $ map (Line . pure . Raw . ("    " <>))
                      $ filter (/= "# or null")
                      $ map (mconcat . map chunkText)
                      $ jsonSchemaChunkLines schema
                  , Lines
                      . pure
                      . Line
                      . ("Default:" :)
                      . pure
                      . Code
                      . fromString
                      <$> confDocDefault
                  ]
            ]
      }
