module Main where

import System.Environment(getArgs)
import Lib

data DocumentContent = TextBlock String | CodeBlock String 
instance Show DocumentContent where
    show (TextBlock s) = "TB: " ++ s
    show (CodeBlock s) = "CB: " ++ s

parseHaskibook :: [String]  -> [DocumentContent]
parseHaskibook lines = p lines [] False  []
    where
        p :: [String] -> [String] -> Bool -> [DocumentContent] -> [DocumentContent]
        p ("```":rest) buffer False dc = p rest buffer True dc
        p ("```":rest) buffer True  dc = p rest [] False (dc ++ [CodeBlock $ unlines buffer])
        p (line:rest)  _      False dc = p rest [] False (dc ++ [TextBlock line])
        p (line:rest)  buffer True  dc = p rest (buffer ++ [line]) True dc
        p []           buffer False dc = dc
        p []           _      True  _  = error "Missing end statement on last code block!"



main :: IO ()
main = do
    args <- getArgs
    let sourceFile = head args
    let resultFile = args!!1

    if null args
        then error  "-!- Missing source file path!"
    else putStrLn $ "--- Opening source file: " ++ sourceFile

    content <- readFile sourceFile
    let l = parseHaskibook $ lines content

    if length args < 2
        then error  "-!- Missing result file path!"
    else putStrLn $ "--- Saving to file: " ++ resultFile

    writeFile resultFile "yoa"
    
    putStrLn "Done!"