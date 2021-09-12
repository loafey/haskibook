{-# LANGUAGE LambdaCase #-}
module Lib (parseHaskibook, ParsedContent(..)) where

data DocumentContent = TextBlock String | CodeBlock String 
data ParsedContent   = Text String | Code (String,String,String) 
    deriving Show


parseHaskibook :: [String]  -> [ParsedContent]
parseHaskibook lines = stringify $ p lines [] False  []
    where
        p :: [String] -> [String] -> Bool -> [DocumentContent] -> [DocumentContent]
        p ("```":rest) buffer False dc = p rest buffer True dc
        p ("```":rest) buffer True  dc = p rest [] False (dc ++ [CodeBlock $ unlines buffer])
        p (line:rest)  _      False dc = p rest [] False (dc ++ [TextBlock line])
        p (line:rest)  buffer True  dc = p rest (buffer ++ [line]) True dc
        p []           buffer False dc = dc
        p []           _      True  _  = error "Missing end statement on last code block!"

stringify :: [DocumentContent] -> [ParsedContent]
stringify = map extract
    where
        extract :: DocumentContent -> ParsedContent
        extract (TextBlock s) = Text s
        extract (CodeBlock s) = parseCode s

data CodeBlockPart = FunctionName String | DataSet String | Haskell String
    deriving Show

parseCode :: String -> ParsedContent
parseCode s = Code (functionName, dataSet, code)
    where 
        parseLine :: String -> CodeBlockPart
        parseLine ('@':'=':rest) = FunctionName rest
        parseLine ('#':'=':rest) = DataSet rest
        parseLine r              = Haskell r

        codeLines = map parseLine $ lines s

        (FunctionName functionName) = head $ filter (\case {FunctionName _ -> True; _ -> False}) codeLines
        (DataSet dataSet) = head $ filter (\case {DataSet _ -> True; _ -> False}) codeLines
        code = unlines $ map (\(Haskell s) -> s) (filter (\case {Haskell _ -> True; _ -> False}) codeLines)