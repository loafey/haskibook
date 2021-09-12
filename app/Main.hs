module Main where

import System.Environment(getArgs)
import System.Process(readProcessWithExitCode, readProcess)
import Lib (parseHaskibook, ParsedContent(..))

main :: IO ()
main = do
    args <- getArgs
    let sourceFile = head args
    let resultFile = args !! 1

    if null args
        then error  "-!- Missing source file path!"
    else putStrLn $ "--- Opening source file: " ++ sourceFile

    content <- readFile sourceFile
    let pc  =  parseHaskibook $ lines content

    if length args < 2
        then error  "-!- Missing result file path!"
    else putStrLn $ "--- Saving to file: " ++ resultFile

    executed <- executeParsedCode pc
    writeFile resultFile executed
    
    putStrLn "Done!"

executeParsedCode :: [ParsedContent] -> IO String
executeParsedCode [] = pure ""
executeParsedCode (Text s: rest) = do
    r <- executeParsedCode rest
    return $ s ++ "\n" ++ r
executeParsedCode (Code (fn, ds, c): rest) = do
    r <- executeParsedCode rest
    let code = c ++ "main = do print $ map show (map " ++ fn ++ " " ++ ds ++ ")"
    -- TODO implement exitcode check
    (_, tempFile, _) <- readProcessWithExitCode "mktemp" ["--suffix=.hs"] ""
    let ghciFile = head $ lines tempFile
    writeFile ghciFile code

    (_, codeOutput, _) <- readProcessWithExitCode "runhaskell" [ghciFile] ""
    return $ codeOutput ++ r