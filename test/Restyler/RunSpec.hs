{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Restyler.RunSpec (spec) where

import SpecHelper

import Restyler.Config
import Restyler.Run
import System.Process (callProcess)

spec :: Spec
spec = around (withSystemTempDirectory "") $ do
    describe "callRestylers" $ do
        it "doesn't run on removed files" $ \dir -> do
            setupGitRepo dir
            setupGitTrackedFile "Foo.hs" "" Nothing
            callProcess "git" ["checkout", "--quiet", "-b", "develop"]
            callProcess "git" ["rm", "Foo.hs"]
            callProcess "git" ["commit", "--quiet", "--message", "Remove file"]
            callRestylers defaultConfig ["Foo.hs"]
            ["Foo.hs"] `shouldRestyleAs` []

        context "Default configuration" $ do
            it "restyles Haskell" $ restylerTestCase "Foo.hs"
                [st|
                    {-# LANGUAGE OverloadedStrings, RecordWildcards
                    #-}
                |]
                [ "-{-# LANGUAGE OverloadedStrings, RecordWildcards"
                , "-#-}"
                , "+{-# LANGUAGE OverloadedStrings #-}"
                , "+{-# LANGUAGE RecordWildcards   #-}"
                ]

            it "restyles JavaScript" $ restylerTestCase "foo.js"
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

        context "non-default configuration" $ do
            describe "hindent" $ do
                it "works" $ \dir -> do
                    setupGitRepo dir
                    setupConfig ["hindent"]
                    setupGitTrackedFile
                        "Foo.hs"
                        "example = case x of Just p -> foo bar\n"
                        $ Just "develop"

                    ["Foo.hs"] `shouldRestyleAs`
                        [ "-example = case x of Just p -> foo bar"
                        , "+example ="
                        , "+  case x of"
                        , "+    Just p -> foo bar"
                        ]

            describe "brittany" $ do
                it "works" $ \dir -> do
                    setupGitRepo dir
                    setupConfig ["brittany"]
                    setupGitTrackedFile
                        "Foo.hs"
                        (dedent [st|
                            func (MyLongFoo abc def) = 1
                            func (Bar a d) = 2
                            func _ = 3
                        |])
                        $ Just "develop"

                    ["Foo.hs"] `shouldRestyleAs`
                        [ " func (MyLongFoo abc def) = 1"
                        , "-func (Bar a d) = 2"
                        , "-func _ = 3"
                        , "+func (Bar       a   d  ) = 2"
                        , "+func _                   = 3"
                        ]

            describe "shfmt" $ do
                it "works" $ \dir -> do
                    setupGitRepo dir
                    setupConfig ["shfmt"]
                    setupGitTrackedFile
                        "foo"
                        (dedent [st|
                            #!/bin/sh
                            if [ 2 -eq 2 ]
                                then
                                    echo "yup"
                                fi
                        |])
                        $ Just "develop"

                    ["foo"] `shouldRestyleAs`
                        [ " #!/bin/sh"
                        , "-if [ 2 -eq 2 ]"
                        , "-    then"
                        , "-        echo \"yup\""
                        , "-    fi"
                        , "+if [ 2 -eq 2 ]; then"
                        , "+\techo \"yup\""
                        , "+fi"
                        ]

            describe "astyle" $ do
                it "works (java)" $ \dir -> do
                    setupGitRepo dir
                    setupConfig ["astyle"]
                    setupGitTrackedFile
                        "Foo.java"
                        (dedent [st|
                            int Foo(bool isBar)
                                {
                                if (isBar) {
                                    bar();
                                    return 1; }
                                else
                                	return 0;
                            }
                        |])
                        $ Just "develop"

                    ["Foo.java"] `shouldRestyleAs`
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

                it "works (cpp)" $ \dir -> do
                    setupGitRepo dir
                    setupConfig ["astyle"]
                    setupGitTrackedFile
                        "Foo.cpp"
                        (dedent [st|
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
                        |])
                        $ Just "develop"

                    ["Foo.cpp"] `shouldRestyleAs`
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

            describe "autopep8" $ do
                it "works" $ \dir -> do
                    setupGitRepo dir
                    setupConfig ["autopep8"]
                    setupGitTrackedFile
                        "foo.py"
                        (dedent [st|
                            import math, sys;
                            def example1():
                                ####This is a long comment. This should be wrapped to fit within 72 characters.
                                some_tuple=(   1,2, 3,'a'  );
                                some_variable={'long':'Long code lines should be wrapped within 79 characters.',
                                'other':[math.pi, 100,200,300,9876543210,'This is a long string that goes on'],
                                'more':{'inner':'This whole logical line should be wrapped.',some_tuple:[1,
                                20,300,40000,500000000,60000000000000000]}}
                                return (some_tuple, some_variable)
                        |])
                        $ Just "develop"

                    ["foo.py"] `shouldRestyleAs`
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

            describe "php-cs-fixer" $ do
                it "works" $ \dir -> do
                    setupGitRepo dir
                    setupConfig ["php-cs-fixer"]
                    setupGitTrackedFile
                        "foo.php"
                        (dedent [st|
                            <?PHP
                            $this->foo();
                        |])
                        $ Just "develop"

                    ["foo.php"] `shouldRestyleAs`
                        [ "-<?PHP"
                        , "+<?php"
                        , " $this->foo();"
                        ]

            describe "elm-format" $ do
                it "works" $ \dir -> do
                    setupGitRepo dir
                    setupConfig ["elm-format"]
                    setupGitTrackedFile
                        "Foo.elm"
                        (dedent [st|
                            homeDirectory = "/root/files"
                            eval boolean = case boolean of
                                Literal bool -> bool
                                Not b        -> not (eval b)
                                And b b_     -> eval b && eval b_
                                Or b b_      -> eval b   || eval b_
                        |])
                        $ Just "develop"

                    ["Foo.elm"] `shouldRestyleAs`
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

restylerTestCase :: FilePath -> Text -> [String] -> FilePath -> Expectation
restylerTestCase name content changes dir = do
    setupGitRepo dir
    setupGitTrackedFile name (dedent content) $ Just "develop"
    [name] `shouldRestyleAs` changes

shouldRestyleAs :: [FilePath] -> [String] -> Expectation
paths `shouldRestyleAs` changes = either
    (expectationFailure . ("loadConfig: " <>))
    (\config -> do
        callRestylers config paths
        output <- lines <$> readProcess "git" ["diff"] ""
        output `shouldContain` changes
    ) =<< loadConfig
