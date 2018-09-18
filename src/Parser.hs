module Parser (
    getNext
) where

import System.IO

data Token = Tnum
           | T_Func String
           | T_Global
           | T_Id String
           | T_Char Char
           | T_If Tcond
           | T_Else
           | T_Int Int
           | T_Return
           | T_Sizeof
           | T_While T_Cond
           | T_Assign
           | T_Cond
           | T_Or
           | T_And
           | T_Xor

skipUntil :: Handle -> Char -> ()
skipUntil handle term = do
    c <- hGetChar handle
    if c == term
        then return ()
        else skipUntil handle term

getNext :: Handle -> IO Token
getNext handle = do
    c <- hGetChar handle
    case c of '\n' -> getNext handle
              '#'  -> skipUntil handle '\n'
