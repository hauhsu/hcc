module Main where

import System.IO
import Parser

main :: IO ()
main = do
    handle <- openFile "test.c" ReadMode
    loop handle
    return ()
            
loop :: Handle -> IO ()
loop handle = do
    isEof <- hIsEOF handle
    if isEof then return ()
    else do
        getNext handle
        loop handle
    
  
