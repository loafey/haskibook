module Main where

import System.Environment(getArgs)
import System.Process(readProcessWithExitCode, readProcess)
import Lib (parseHaskibook, ParsedContent(..))

main :: IO ()
main = getArgs >>= checkArgs >>= execute
    where execute (s,r) = getContent s >>= executeParsedCode >>= writeFile r

checkArgs :: [String] -> IO(String, String)
checkArgs [s,r] = pure (s,r)
checkArgs []    = error "Missing source file path!"
checkArgs [_]   = error "Missing result file path!"
checkArgs (s : (r : _)) = pure (s,r)

getContent :: String -> IO [ParsedContent]
getContent s = readFile s >>= (\x -> pure (parseHaskibook $ lines x))


executeParsedCode :: [ParsedContent] -> IO String
executeParsedCode [] = pure ""
executeParsedCode (Text s: rest) = do
    r <- executeParsedCode rest
    return (s ++ "\n" ++ r)
executeParsedCode (Code (fn, ds, c): rest) = do
    r <- executeParsedCode rest
    let code = c ++ "main = do print $ map show (map " ++ fn ++ " " ++ ds ++ ")"
    -- TODO implement exitcode check
    (_, tempFile, _) <- readProcessWithExitCode "mktemp" ["--suffix=.hs"] ""
    let ghciFile = head $ lines tempFile
    writeFile ghciFile code

    (_, codeOutput, _) <- readProcessWithExitCode "runhaskell" [ghciFile] ""
    return (codeOutput ++ r)