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

data Operation =
    Username |
    Hostname |
    PWD | 
    Color ColorCode
    deriving Show

data PromptTokens = Op Operation | Filler String
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


parseOperation :: Parser Operation
parseOperation = let 
        matchOp s =  case s of 
            "Username" -> Username
            "Hostname" -> Hostname
            "PWD" -> PWD
            in do 
                char startChar
                str <- manyTill anyChar (char endChar)
                return $ matchOp str

tokenList :: Parser [PromptTokens]
tokenList = let
        filler = Filler <$> many1 (noneOf [startChar])
        op = Op <$> parseOperation
    in many (filler <|> op)

main :: IO ()
main = do
    input <- getArgs
    -- parseTest moreTokens (head input)
    parseTest tokenList (head input)
    -- putStrLn $ show (regularParse moreTokens (head input))
