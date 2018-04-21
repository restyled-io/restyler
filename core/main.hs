module Main (main) where

import Restyler.Core (restyle)
import System.Environment (getArgs)

main :: IO ()
main = restyle =<< getArgs
