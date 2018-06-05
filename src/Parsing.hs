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
parseColor str = let (color, ctype) = span (','/=) str in
    parseColor' color (getType ctype)
    where
    getType ctype = 
        if null ctype then Normal
        else case toLowerStr (drop 1 ctype) of 
            "normal" -> Normal
            "bold" -> Bold 
            "underline" -> Underline 
            "background" -> Background 
            "highintensity" -> HighIntensity 
            "boldhighintensity" -> BoldHighIntensity 
            "highintensitybackground" -> HighIntensityBackground
    parseColor' str ct = 
        case (toLowerStr str) of
            "red" -> Red ct
            "blue" -> Blue ct
            "green" -> Green ct
            "yellow" -> Yellow ct
            "black" -> Black ct
            "white" -> White ct
            "purple" -> Purple ct
            "cyan" -> Cyan ct

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