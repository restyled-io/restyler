  $ source "$TESTDIR/helper.sh"
  If you don't see this, stack install failed

stylish-haskell

  $ run_restyler stylish-haskell pragmas.hs
  [Info] Restyling ["pragmas.hs"] via stylish-haskell
  diff --git i/pragmas.hs w/pragmas.hs
  index fd4c8c8..a07dc58 100644
  --- i/pragmas.hs
  +++ w/pragmas.hs
  @@ -1,2 +1,2 @@
   {-# LANGUAGE OverloadedStrings #-}
  -{-# LANGUAGE RecordWildCards #-}
  +{-# LANGUAGE RecordWildCards   #-}

prettier

  $ run_restyler prettier matrix.js
  [Info] Restyling ["matrix.js"] via prettier
  * (glob)
  diff --git i/matrix.js w/matrix.js
  index 430121c..811d19c 100644
  --- i/matrix.js
  +++ w/matrix.js
  @@ -1,5 +1 @@
  -matrix(
  -  1, 0, 0,
  -  0, 1, 0,
  -  0, 0, 1
  -)
  +matrix(1, 0, 0, 0, 1, 0, 0, 0, 1);

hindent

  $ run_restyler hindent case.hs
  [Info] Restyling ["case.hs"] via hindent
  diff --git i/case.hs w/case.hs
  index 08aded8..823b4bf 100644
  --- i/case.hs
  +++ w/case.hs
  @@ -1 +1,3 @@
  -example = case x of Just p -> foo bar
  +example =
  +  case x of
  +    Just p -> foo bar

brittany

  $ run_restyler brittany patterns.hs
  [Info] Restyling ["patterns.hs"] via brittany
  diff --git i/patterns.hs w/patterns.hs
  index 169b4f8..198bd69 100644
  --- i/patterns.hs
  +++ w/patterns.hs
  @@ -1,3 +1,3 @@
   func (MyLongFoo abc def) = 1
  -func (Bar a d) = 2
  -func _ = 3
  +func (Bar       a   d  ) = 2
  +func _                   = 3

shfmt

  $ run_restyler shfmt foo-script
  [Info] Restyling ["foo-script"] via shfmt
  diff --git i/foo-script w/foo-script
  index bcbb874..2fc3db2 100644
  --- i/foo-script
  +++ w/foo-script
  @@ -1,5 +1,4 @@
   #!/bin/sh
  -if [ 2 -eq 2 ]
  -    then
  -        echo "yup"
  -    fi
  +if [ 2 -eq 2 ]; then
  +\techo "yup" (esc)
  +fi

astyle

  $ run_restyler astyle Foo.java Foo.cpp
  [Info] Restyling ["Foo.java","Foo.cpp"] via astyle
  Formatted  ./Foo.java
  Formatted  ./Foo.cpp
  diff --git i/Foo.cpp w/Foo.cpp
  index f5680da..df055a4 100644
  --- i/Foo.cpp
  +++ w/Foo.cpp
  @@ -2,16 +2,16 @@
   #include <stdio.h>
   int main()
   {
  -   FILE * pFile;
  -   char buffer [100];
  -   pFile = fopen ("myfile.txt" , "r");
  -   if (pFile == NULL) perror ("Error opening file");
  -   else {
  -     while ( ! feof (pFile) ) {
  -       if ( fgets (buffer , 100 , pFile) == NULL ) break;
  -       fputs (buffer , stdout);
  -     }
  -     fclose (pFile);
  -   }
  -   return 0;
  +    FILE * pFile;
  +    char buffer [100];
  +    pFile = fopen ("myfile.txt", "r");
  +    if (pFile == NULL) perror ("Error opening file");
  +    else {
  +        while ( ! feof (pFile) ) {
  +            if ( fgets (buffer, 100, pFile) == NULL ) break;
  +            fputs (buffer, stdout);
  +        }
  +        fclose (pFile);
  +    }
  +    return 0;
   }
  diff --git i/Foo.java w/Foo.java
  index 805238c..45b86bf 100644
  --- i/Foo.java
  +++ w/Foo.java
  @@ -1,8 +1,9 @@
   int Foo(bool isBar)
  -    {
  +{
       if (isBar) {
           bar();
  -        return 1; }
  +        return 1;
  +    }
       else
  -    \treturn 0; (esc)
  +        return 0;
   }

autopep8

  $ run_restyler autopep8 crazy.py
  [Info] Restyling ["crazy.py"] via autopep8
  diff --git i/crazy.py w/crazy.py
  index 9941de7..6451f03 100644
  --- i/crazy.py
  +++ w/crazy.py
  @@ -1,9 +1,12 @@
  -import math, sys;
  +import math
  +import sys
  +
  +
   def example1():
  -    ####This is a long comment. This should be wrapped to fit within 72 characters.
  -    some_tuple=(   1,2, 3,'a'  );
  -    some_variable={'long':'Long code lines should be wrapped within 79 characters.',
  -    'other':[math.pi, 100,200,300,9876543210,'This is a long string that goes on'],
  -    'more':{'inner':'This whole logical line should be wrapped.',some_tuple:[1,
  -    20,300,40000,500000000,60000000000000000]}}
  +    # This is a long comment. This should be wrapped to fit within 72 characters.
  +    some_tuple = (1, 2, 3, 'a')
  +    some_variable = {'long': 'Long code lines should be wrapped within 79 characters.',
  +                     'other': [math.pi, 100, 200, 300, 9876543210, 'This is a long string that goes on'],
  +                     'more': {'inner': 'This whole logical line should be wrapped.', some_tuple: [1,
  +                                                                                                  20, 300, 40000, 500000000, 60000000000000000]}}
       return (some_tuple, some_variable)

php-cs-fixer

  $ run_restyler php-cs-fixer tag.php
  [Info] Restyling ["tag.php"] via php-cs-fixer
  Loaded config default.
     1) tag.php
  
  Fixed all files in 0.004 seconds, 8.000 MB memory used
  diff --git i/tag.php w/tag.php
  index c7ca2e5..41a4dca 100644
  --- i/tag.php
  +++ w/tag.php
  @@ -1,2 +1,2 @@
  -<?PHP
  +<?php
   $this->foo();

elm-format

  $ run_restyler elm-format case.elm
  [Info] Restyling ["case.elm"] via elm-format
  Processing file ./case.elm
  diff --git i/case.elm w/case.elm
  index e4654af..65d084f 100644
  --- i/case.elm
  +++ w/case.elm
  @@ -1,6 +1,20 @@
  -homeDirectory = "/root/files"
  -eval boolean = case boolean of
  -    Literal bool -> bool
  -    Not b        -> not (eval b)
  -    And b b_     -> eval b && eval b_
  -    Or b b_      -> eval b   || eval b_
  +module Main exposing (..)
  +
  +
  +homeDirectory =
  +    "/root/files"
  +
  +
  +eval boolean =
  +    case boolean of
  +        Literal bool ->
  +            bool
  +
  +        Not b ->
  +            not (eval b)
  +
  +        And b b_ ->
  +            eval b && eval b_
  +
  +        Or b b_ ->
  +            eval b || eval b_

rubocop

  $ run_restyler rubocop four.rb
  [Info] Restyling ["four.rb"] via rubocop
  Inspecting 1 file
  C
  
  Offenses:
  
  four.rb:3:1: C: [Corrected] Layout/IndentationWidth: Use 2 (not 4) spaces for indentation.
      do_something
  ^^^^
  
  1 file inspected, 1 offense detected, 1 offense corrected
  diff --git i/four.rb w/four.rb
  index bcea85b..04d7c60 100644
  --- i/four.rb
  +++ w/four.rb
  @@ -1,4 +1,4 @@
   # bad - four spaces
   def some_method
  -    do_something
  +  do_something
   end

rustfmt

  $ run_restyler rustfmt example.rs
  [Info] Restyling ["example.rs"] via rustfmt
  diff --git i/example.rs w/example.rs
  index 639567b..a5d4acc 100644
  --- i/example.rs
  +++ w/example.rs
  @@ -1,4 +1,5 @@
   // Attributes should be on their own lines
   struct CRepr {
  -    x: f32,y: f32,
  +    x: f32,
  +    y: f32,
   }
