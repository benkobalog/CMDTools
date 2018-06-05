module Interpret
    ( interpretList
    ) where

import PromptModel

colorBlock s ct = let str = colorTypeModifier ct s in
    "\\[\\033[" ++ str ++ "m\\]"

colorTypeModifier :: ColorType -> Int -> String
colorTypeModifier ct num = 
    case ct of
        Normal -> "0;" ++ (show num)
        Bold -> "1;" ++ (show num)
        Underline -> "4;" ++ (show num) -- This probably won't work
        Background -> "1;" ++ (show (num + 10))
        HighIntensity -> "0;" ++ (show (num + 60))
        BoldHighIntensity -> "1;" ++ (show (num + 60))
        HighIntensityBackground -> "0;" ++ (show (num + 70))

class PromptListInterpreter a where
    interpret :: a -> String

instance PromptListInterpreter ColorCode where
    interpret cc = case cc of
        Black ct ->  colorBlock 30 ct
        Red ct ->    colorBlock 31 ct
        Green ct ->  colorBlock 32 ct
        Yellow ct -> colorBlock 33 ct
        Blue ct ->   colorBlock 34 ct
        Purple ct -> colorBlock 34 ct
        Cyan ct ->   colorBlock 36 ct
        White ct ->  colorBlock 37 ct

instance PromptListInterpreter PromptPart where
    interpret pp = case pp of
        Username -> "\\u"
        Hostname -> "\\H"
        PWD -> "\\w"
        Color cc -> interpret cc

instance PromptListInterpreter PromptTokens where
    interpret (Part pp) = interpret pp
    interpret (Filler str) = str

interpretList :: [PromptTokens] -> String
interpretList xs = 
    (foldl (++) "" (fmap interpret (xs))) ++ "\\e[0m"