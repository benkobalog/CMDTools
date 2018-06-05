module PromptModel
    ( ColorCode (..)
    , PromptPart (..)
    , PromptTokens (..)
    ) where

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