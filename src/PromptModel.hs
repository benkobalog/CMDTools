module PromptModel
    ( ColorCode (..)
    , PromptPart (..)
    , PromptTokens (..)
    , ColorType (..)
    ) where

data ColorCode =
    Red ColorType |
    Blue ColorType |
    Green ColorType |
    Yellow ColorType |
    Black ColorType |
    White ColorType |
    Purple ColorType |
    Cyan ColorType
    deriving Show

data ColorType = 
    Normal |
    Bold |
    Underline |
    Background |
    HighIntensity |
    BoldHighIntensity |
    HighIntensityBackground
    deriving Show

data PromptPart =
    Username |
    Hostname |
    PWD | 
    Color ColorCode
    deriving Show

data PromptTokens = Part PromptPart | Filler String
    deriving Show