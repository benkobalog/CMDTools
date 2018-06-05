module Interpret
    ( interpretList
    ) where

import PromptModel

colorBlock s = "\\[\\033[" ++ s ++ "m\\]"

class PromptListInterpreter a where
    interpret :: a -> String

instance PromptListInterpreter ColorCode where
    interpret cc = case cc of
        Red -> colorBlock "31"
        Blue -> colorBlock "34"
        Green -> colorBlock "32"
        Yellow -> colorBlock "33"
        Black -> colorBlock "30"
        White -> colorBlock "37"
        Code int -> colorBlock (show int)

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