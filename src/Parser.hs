module Parser (
    getNext
) where

import System.IO

data Token = Tnum
           | T_Func String
           | T_Global
           | T_Id String
           | T_Char Char
           | T_Cond
           | T_If
           | T_Else
           | T_Int Int
           | T_Return
           | T_Sizeof
           | T_While
           | T_Assign
           | T_Or
           | T_And
           | T_Xor
           | T_EOF


skipUntil :: Handle -> Char -> IO()
skipUntil handle term = do
    c <- hGetChar handle
    putChar c
    if c == term
        then return ()
        else skipUntil handle term

readUntil :: Handle -> [Char] -> IO [Char]
readUntil handle terms = do
    c <- hGetChar handle
    if not (c `elem` terms)
        then do
            t <- (readUntil handle terms)
            return ([c] ++ t)
        else return ""

terminalChars = ['\n', ' ', '=', '+', '-', '*']
handleWords :: Char -> Handle -> IO Token
handleWords c handle = do
    word <- readUntil handle terminalChars
    putStrLn $ "Read word: " ++ [c] ++ word
    return $ T_Id word

getNext :: Handle -> IO Token
getNext handle = do
    c <- hGetChar handle
    case c of '\n'  -> getNext handle
              '#'   -> do
                        putStr "Skipping macro:\n#"
                        skipUntil handle '\n'
                        getNext handle
              _     -> handleWords c handle
