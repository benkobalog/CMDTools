module Main where

import Lib
import Text.Parsec
import Text.Parsec.String
import System.Environment
import Data.Char

toLowerStr str = map toLower str

data ColorCode =
    Red |
    Blue |
    Green |
    Yellow |
    Black |
    White |
    Code Int
    deriving Show

data PromptPart =
    Username |
    Hostname |
    PWD | 
    Color ColorCode
    deriving Show

data PromptTokens = Part PromptPart | Filler String
    deriving Show

startChar = '{'
endChar = '}'

parseColor :: String -> ColorCode
parseColor str = 
    case (toLowerStr str) of
        "red" -> Red
        "blue" -> Blue
        "green" -> Green 
        "yellow" -> Yellow 
        "black" -> Black 
        "white" -> White
        s -> extractColor s 
    where
        extractColor ('c':'o':'l':'o':'r':' ':code) = Code (read code)


parseOperation :: Parser PromptPart
parseOperation = let 
        matchOp s = case s of 
            "Username" -> Username
            "Hostname" -> Hostname
            "PWD" -> PWD
            rest -> Color $ parseColor rest
    in do
        char startChar
        str <- manyTill anyChar (char endChar)
        return $ matchOp str

tokenList :: Parser [PromptTokens]
tokenList = let
        filler = Filler <$> many1 (noneOf [startChar])
        part = Part <$> parseOperation
    in many (filler <|> part)

main :: IO ()
main = do
    input <- getArgs
    let result = parse tokenList "" (head input) in
        case result of
            Right tokenList -> putStrLn $ show tokenList
            Left err -> putStrLn $ show err