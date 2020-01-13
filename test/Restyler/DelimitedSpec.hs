module Restyler.DelimitedSpec
    ( spec
    )
where

import SpecHelper

import qualified Data.Text as T
import Restyler.App.Class
import Restyler.Delimited

spec :: Spec
spec = do
    describe "restyleDelimited" $ do
        it "restyles delimited content" $ runTestApp $ do
            writeFile "foo.rb" $ T.unlines
                [ "def some_ruby"
                , "  <<-EOSQL"
                , "    SELECT"
                , "    FROM"
                , "  EOSQL"
                , "end"
                ]

            restyleDelimited
                (Delimiters "<<-EOSQL" "EOSQL")
                markLinesRestyled
                ["foo.rb"]

            readFile "foo.rb" `shouldReturn` T.unlines
                [ "def some_ruby"
                , "  <<-EOSQL"
                , "    RESTYLED: SELECT"
                , "    RESTYLED: FROM"
                , "  EOSQL"
                , "end"
                ]

    describe "delimit" $ do
        it "splits a file, respecting indentation" $ runTestApp $ do
            writeFile "foo.rb" $ T.unlines
                [ "def some_ruby"
                , "  <<-EOSQL"
                , "    SELECT"
                , "    FROM"
                , "  EOSQL"
                , "end"
                ]

            result <- delimit (Delimiters "<<-EOSQL" "EOSQL") "foo.rb"

            result `shouldBe` DelimitedPath
                { dpSource = "foo.rb"
                , dpParts =
                    [ outPart "foo.rb.0"
                    , inPart "foo.rb.1" "\n" "  " 4
                    , outPart "foo.rb.2"
                    ]
                }
            readFile "foo.rb.0" `shouldReturn` "def some_ruby\n  "
            readFile "foo.rb.1" `shouldReturn` "SELECT\nFROM\n"
            readFile "foo.rb.2" `shouldReturn` "\nend\n"

    describe "undelimit" $ do
        it "can reconstruct a delimited path" $ runTestApp $ do
            writeFile "foo.rb.0" "def some_ruby\n  "
            writeFile "foo.rb.1" "SELECT\nFROM\n"
            writeFile "foo.rb.2" "\nend\n"

            undelimit
                (Delimiters "<<-EOSQL" "EOSQL")
                DelimitedPath
                    { dpSource = "foo.rb"
                    , dpParts =
                        [ outPart "foo.rb.0"
                        , inPart "foo.rb.1" "\n" "  " 4
                        , outPart "foo.rb.2"
                        ]
                    }

            readFile "/foo.rb" `shouldReturn` T.unlines
                [ "def some_ruby"
                , "  <<-EOSQL"
                , "    SELECT"
                , "    FROM"
                , "  EOSQL"
                , "end"
                ]

markLinesRestyled :: HasSystem env => [FilePath] -> RIO env ()
markLinesRestyled = traverse_
    $ \path -> writeFile path . mark =<< readFile path
    where mark = T.unlines . map ("RESTYLED: " <>) . T.lines

outPart :: FilePath -> DelimitedPathPart
outPart path =
    DelimitedPathPart { dppIn = False, dppPath = path, dppMeta = Nothing }

inPart :: FilePath -> Text -> Text -> Natural -> DelimitedPathPart
inPart path leading trailing indent = DelimitedPathPart
    { dppIn = True
    , dppPath = path
    , dppMeta = Just DelimitedMeta
        { dmLeading = leading
        , dmTrailing = trailing
        , dmIndent = indent
        }
    }
