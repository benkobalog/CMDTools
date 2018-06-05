module Main where

import Parsing
import Text.Parsec
import System.Environment
import Data.List
import Interpret

main :: IO ()
main = do
    input <- getArgs
    let promptString = intercalate "" input
        result = parse tokenList "" promptString in
        case result of
            Right tokenList -> putStrLn $ interpretList tokenList
            Left err -> putStrLn $ show err