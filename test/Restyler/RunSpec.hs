{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Restyler.RunSpec (spec) where

import SpecHelper

import Restyler.Config
import Restyler.Run

spec :: Spec
spec = around (withSystemTempDirectory "") $ do
    describe "callRestylers" $ do
        it "doesn't run on removed files" $ \dir -> do
            setupGitRepo dir
            setupGitTrackedFile "Foo.hs" "" Nothing
            callProcess "git" ["checkout", "-b", "develop"]
            callProcess "git" ["rm", "Foo.hs"]
            callProcess "git" ["commit", "--message", "Remove file"]
            callRestylers defaultConfig ["Foo.hs"]
            "Foo.hs" `shouldRestyleAs` []

        it "restyles Haskell by default" $ restylerTestCase [] "Foo.hs"
            [st|
                {-# LANGUAGE OverloadedStrings, RecordWildcards
                #-}
            |]
            [ "-{-# LANGUAGE OverloadedStrings, RecordWildcards"
            , "-#-}"
            , "+{-# LANGUAGE OverloadedStrings #-}"
            , "+{-# LANGUAGE RecordWildcards   #-}"
            ]

        it "restyles JavaScript by default" $ restylerTestCase [] "foo.js"
            [st|
                matrix(
                  1, 0, 0,
                  0, 1, 0,
                  0, 0, 1
                )
            |]
            [ "-matrix("
            , "-  1, 0, 0,"
            , "-  0, 1, 0,"
            , "-  0, 0, 1"
            , "-)"
            , "+matrix(1, 0, 0, 0, 1, 0, 0, 0, 1);"
            ]

        it "runs hindent" $ restylerTestCase ["hindent"] "Foo.hs"
            [st|
                example = case x of Just p -> foo bar
            |]
            [ "-example = case x of Just p -> foo bar"
            , "+example ="
            , "+  case x of"
            , "+    Just p -> foo bar"
            ]

        it "runs brittany" $ restylerTestCase ["brittany"] "Foo.hs"
            [st|
                func (MyLongFoo abc def) = 1
                func (Bar a d) = 2
                func _ = 3
            |]
            [ " func (MyLongFoo abc def) = 1"
            , "-func (Bar a d) = 2"
            , "-func _ = 3"
            , "+func (Bar       a   d  ) = 2"
            , "+func _                   = 3"
            ]

        it "runs shfmt" $ restylerTestCase ["shfmt"] "foo"
            [st|
                #!/bin/sh
                if [ 2 -eq 2 ]
                    then
                        echo "yup"
                    fi
            |]
            [ " #!/bin/sh"
            , "-if [ 2 -eq 2 ]"
            , "-    then"
            , "-        echo \"yup\""
            , "-    fi"
            , "+if [ 2 -eq 2 ]; then"
            , "+\techo \"yup\""
            , "+fi"
            ]

        it "astyles java" $ restylerTestCase ["astyle"] "Foo.java"
            [st|
                int Foo(bool isBar)
                    {
                    if (isBar) {
                        bar();
                        return 1; }
                    else
                    	return 0;
                }
            |]
            [ " int Foo(bool isBar)"
            , "-    {"
            , "+{"
            , "     if (isBar) {"
            , "         bar();"
            , "-        return 1; }"
            , "+        return 1;"
            , "+    }"
            , "     else"
            , "-    \treturn 0;"
            , "+        return 0;"
            , " }"
            ]

        it "astyles cpp" $ restylerTestCase ["astyle"] "Foo.cpp"
            [st|
                /* FEOF example */
                #include <stdio.h>
                int main()
                {
                   FILE * pFile;
                   char buffer [100];
                   pFile = fopen ("myfile.txt" , "r");
                   if (pFile == NULL) perror ("Error opening file");
                   else {
                     while ( ! feof (pFile) ) {
                       if ( fgets (buffer , 100 , pFile) == NULL ) break;
                       fputs (buffer , stdout);
                     }
                     fclose (pFile);
                   }
                   return 0;
                }
            |]
            [ " #include <stdio.h>"
            , " int main()"
            , " {"
            , "-   FILE * pFile;"
            , "-   char buffer [100];"
            , "-   pFile = fopen (\"myfile.txt\" , \"r\");"
            , "-   if (pFile == NULL) perror (\"Error opening file\");"
            , "-   else {"
            , "-     while ( ! feof (pFile) ) {"
            , "-       if ( fgets (buffer , 100 , pFile) == NULL ) break;"
            , "-       fputs (buffer , stdout);"
            , "-     }"
            , "-     fclose (pFile);"
            , "-   }"
            , "-   return 0;"
            , "+    FILE * pFile;"
            , "+    char buffer [100];"
            , "+    pFile = fopen (\"myfile.txt\", \"r\");"
            , "+    if (pFile == NULL) perror (\"Error opening file\");"
            , "+    else {"
            , "+        while ( ! feof (pFile) ) {"
            , "+            if ( fgets (buffer, 100, pFile) == NULL ) break;"
            , "+            fputs (buffer, stdout);"
            , "+        }"
            , "+        fclose (pFile);"
            , "+    }"
            , "+    return 0;"
            , " }"
            ]

        it "runs autopep8" $ restylerTestCase ["autopep8"] "foo.py"
            [st|
                import math, sys;
                def example1():
                    ####This is a long comment. This should be wrapped to fit within 72 characters.
                    some_tuple=(   1,2, 3,'a'  );
                    some_variable={'long':'Long code lines should be wrapped within 79 characters.',
                    'other':[math.pi, 100,200,300,9876543210,'This is a long string that goes on'],
                    'more':{'inner':'This whole logical line should be wrapped.',some_tuple:[1,
                    20,300,40000,500000000,60000000000000000]}}
                    return (some_tuple, some_variable)
            |]
            [ "-import math, sys;"
            , "+import math"
            , "+import sys"
            , "+"
            , "+"
            , " def example1():"
            , "-    ####This is a long comment. This should be wrapped to fit within 72 characters."
            , "-    some_tuple=(   1,2, 3,'a'  );"
            , "-    some_variable={'long':'Long code lines should be wrapped within 79 characters.',"
            , "-    'other':[math.pi, 100,200,300,9876543210,'This is a long string that goes on'],"
            , "-    'more':{'inner':'This whole logical line should be wrapped.',some_tuple:[1,"
            , "-    20,300,40000,500000000,60000000000000000]}}"
            , "+    # This is a long comment. This should be wrapped to fit within 72 characters."
            , "+    some_tuple = (1, 2, 3, 'a')"
            , "+    some_variable = {'long': 'Long code lines should be wrapped within 79 characters.',"
            , "+                     'other': [math.pi, 100, 200, 300, 9876543210, 'This is a long string that goes on'],"
            , "+                     'more': {'inner': 'This whole logical line should be wrapped.', some_tuple: [1,"
            , "+                                                                                                  20, 300, 40000, 500000000, 60000000000000000]}}"
            , "     return (some_tuple, some_variable)"
            ]

        it "runs php-cs-fixer" $ restylerTestCase ["php-cs-fixer"] "foo.php"
            [st|
                <?PHP
                $this->foo();
            |]
            [ "-<?PHP"
            , "+<?php"
            , " $this->foo();"
            ]

        it "runs elm-format" $ restylerTestCase ["elm-format"] "Foo.elm"
            [st|
                homeDirectory = "/root/files"
                eval boolean = case boolean of
                    Literal bool -> bool
                    Not b        -> not (eval b)
                    And b b_     -> eval b && eval b_
                    Or b b_      -> eval b   || eval b_
            |]
            [ "-homeDirectory = \"/root/files\""
            , "-eval boolean = case boolean of"
            , "-    Literal bool -> bool"
            , "-    Not b        -> not (eval b)"
            , "-    And b b_     -> eval b && eval b_"
            , "-    Or b b_      -> eval b   || eval b_"
            , "+module Main exposing (..)"
            , "+"
            , "+"
            , "+homeDirectory ="
            , "+    \"/root/files\""
            , "+"
            , "+"
            , "+eval boolean ="
            , "+    case boolean of"
            , "+        Literal bool ->"
            , "+            bool"
            , "+"
            , "+        Not b ->"
            , "+            not (eval b)"
            , "+"
            , "+        And b b_ ->"
            , "+            eval b && eval b_"
            , "+"
            , "+        Or b b_ ->"
            , "+            eval b || eval b_"
            ]

        it "runs rubocop" $ restylerTestCase ["rubocop"] "foo.rb"
            [st|
                # bad - four spaces
                def some_method
                    do_something
                end
            |]
            [ " # bad - four spaces"
            , " def some_method"
            , "-    do_something"
            , "+  do_something"
            , " end"
            ]

        it "runs rustfmt" $ restylerTestCase ["rustfmt"] "foo.rs"
            [st|
                // Attributes should be on their own lines
                struct CRepr {
                    x: f32,y: f32,
                }
            |]
            [ " // Attributes should be on their own lines"
            , " struct CRepr {"
            , "-    x: f32,y: f32,"
            , "+    x: f32,"
            , "+    y: f32,"
            , " }"
            ]

restylerTestCase
    :: [Text]   -- ^ Restylers, leave empty to use default configuration
    -> FilePath -- ^ Filename to restyle
    -> Text     -- ^ Content of pre-restyled file
    -> [String] -- ^ Lines of diff to expect
    -> FilePath -- ^ The temporary directory to use
    -> Expectation
restylerTestCase restylers name content changes dir = do
    setupGitRepo dir
    unless (null restylers) $ setupConfig restylers
    setupGitTrackedFile name (dedent content) $ Just "develop"
    name `shouldRestyleAs` changes

shouldRestyleAs :: FilePath -> [String] -> Expectation
path `shouldRestyleAs` changes = either
    (expectationFailure . ("loadConfig: " <>))
    (\config -> do
        callRestylers config [path]
        output <- lines <$> readProcess "git" ["diff"] ""
        output `shouldContain` changes
    ) =<< loadConfig
