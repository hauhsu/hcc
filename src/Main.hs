module Main where

import System.IO
import Parser

main :: IO ()
main = do
    handle <- openFile "test.c" Readmode
    
  
