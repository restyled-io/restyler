module Restyler.Docs
  ( DocsPage (..)
  , renderDocsPage
  )
where

import Restyler.Prelude

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Text.IO qualified as T
import OptEnvConf.Parser (Parser)
import Ronn
import Ronn.OptEnvConf ()

data DocsPage = Restyle1 | RestyledYaml5 ByteString

renderDocsPage :: DocsPage -> Parser a -> IO b
renderDocsPage docsPage p = do
  T.putStrLn $ formatDocsPage docsPage p
  exitSuccess

formatDocsPage :: DocsPage -> Parser a -> Text
formatDocsPage = \case
  Restyle1 -> ronnToText . ronnRestyle1
  RestyledYaml5 yaml -> ronnToText . ronnRestyledYaml5 yaml

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
                    { name = ".restyled.yaml"
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

ronnRestyledYaml5 :: ByteString -> Parser a -> Ronn
ronnRestyledYaml5 yaml _ =
  Ronn
    { name = ManRef "restyled.yaml" ManSection5
    , description = ["Restyled configuration file"]
    , sections =
        [ Section
            { name = "SYNOPSIS"
            , content =
                [ Groups
                    [ Lines
                        [ Line
                            [ "Restyled configuration, loaded from the first of"
                            , mconcat
                                $ intersperse ", "
                                $ map
                                  Code
                                  [ ".restyled.yaml"
                                  , ".restyled.yml"
                                  , ".github/restyled.yaml"
                                  , ".github/restyled.yml"
                                  ]
                            , "found"
                            ]
                        ]
                    ]
                ]
            }
        , Section
            { name = "DESCRIPTION"
            , content = [Groups [Lines $ indentLines yaml]]
            }
        ]
    }

indentLines :: ByteString -> [Line]
indentLines = map (Line . pure . Raw . decodeUtf8 . indentLine) . BS8.lines

indentLine :: ByteString -> ByteString
indentLine bs
  | BS.null bs = bs
  | otherwise = "    " <> bs
