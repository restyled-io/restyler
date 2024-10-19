-- |
--
-- Module      : Restyler.DelimitedSpec
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restyler.DelimitedSpec
  ( spec
  ) where

import Restyler.Prelude

import Data.Text qualified as T
import Restyler.Delimited
import Restyler.Test.App
import Restyler.Test.FS (FS)
import Restyler.Test.FS qualified as FS

withFS :: SpecWith FS -> Spec
withFS = before $ FS.build "/" []

spec :: Spec
spec = withFS $ do
  describe "restyleDelimited" $ do
    it "restyles delimited content" $ do
      writeFile "foo.rb"
        $ T.unlines
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

      readFile "foo.rb"
        `shouldReturn` T.unlines
          [ "def some_ruby"
          , "  <<-EOSQL"
          , "    RESTYLED: SELECT"
          , "    RESTYLED: FROM"
          , "  EOSQL"
          , "end"
          ]

      -- Test cleanup
      doesFileExist "foo.rb.0" `shouldReturn` False

    it "works for markdown lists" $ do
      writeFile "foo.md"
        $ T.unlines
          [ "1. A request API Type should be named `Api{Action}{Resource}({Target})`."
          , ""
          , "   ```hs"
          , "   -- Good (Target omitted, acting on entire resource)"
          , "   data ApiCreateTeacher"
          , "   data ApiDeleteTeacher"
          , ""
          , "   -- Good (Target included)"
          , "   data ApiSetTeacherPassword"
          , ""
          , "   -- Bad"
          , "   data TeacherPUT"
          , "   data CreateApiTeacher"
          , "   data ApiTeacherSetPassword"
          , "   ```"
          ]

      restyleDelimited
        (Delimiters "```hs" "```")
        markLinesRestyled
        ["foo.md"]

      readFile "foo.md"
        `shouldReturn` T.unlines
          [ "1. A request API Type should be named `Api{Action}{Resource}({Target})`."
          , ""
          , "   ```hs"
          , "   RESTYLED: -- Good (Target omitted, acting on entire resource)"
          , "   RESTYLED: data ApiCreateTeacher"
          , "   RESTYLED: data ApiDeleteTeacher"
          , "   RESTYLED: "
          , "   RESTYLED: -- Good (Target included)"
          , "   RESTYLED: data ApiSetTeacherPassword"
          , "   RESTYLED: "
          , "   RESTYLED: -- Bad"
          , "   RESTYLED: data TeacherPUT"
          , "   RESTYLED: data CreateApiTeacher"
          , "   RESTYLED: data ApiTeacherSetPassword"
          , "   ```"
          ]

  describe "delimit" $ do
    it "splits a file, respecting indentation" $ do
      writeFile "foo.rb"
        $ T.unlines
          [ "def some_ruby"
          , "  <<-EOSQL"
          , "    SELECT"
          , "    FROM"
          , "  EOSQL"
          , "end"
          ]

      result <- delimit (Delimiters "<<-EOSQL" "EOSQL") "foo.rb"

      result
        `shouldBe` DelimitedPath
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
    it "can reconstruct a delimited path" $ do
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

      readFile "/foo.rb"
        `shouldReturn` T.unlines
          [ "def some_ruby"
          , "  <<-EOSQL"
          , "    SELECT"
          , "    FROM"
          , "  EOSQL"
          , "end"
          ]

markLinesRestyled
  :: (MonadDirectory m, MonadReadFile m, MonadWriteFile m) => [FilePath] -> m ()
markLinesRestyled = traverse_
  $ \path -> writeFile path . mark =<< readFile path
 where
  mark = T.unlines . map ("RESTYLED: " <>) . T.lines

outPart :: FilePath -> DelimitedPathPart
outPart path =
  DelimitedPathPart {dppIn = False, dppPath = path, dppMeta = Nothing}

inPart :: FilePath -> Text -> Text -> Natural -> DelimitedPathPart
inPart path leading trailing indent =
  DelimitedPathPart
    { dppIn = True
    , dppPath = path
    , dppMeta =
        Just
          DelimitedMeta
            { dmLeading = leading
            , dmTrailing = trailing
            , dmIndent = indent
            }
    }
