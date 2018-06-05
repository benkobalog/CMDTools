module Parsing
    ( tokenList
    ) where

import Text.Parsec
import Text.Parsec.String
import Data.Char
import PromptModel

startChar = '{'
endChar = '}'

toLowerStr str = map toLower str

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
        extractColor ('c':'o':'l':'o':'r':' ':code) = 
            Code (read code)

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