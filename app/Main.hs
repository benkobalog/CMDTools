module Main where

import Parsing
import Text.Parsec
import System.Environment

main :: IO ()
main = do
    input <- getArgs
    let result = parse tokenList "" (head input) in
        case result of
            Right tokenList -> putStrLn $ show tokenList
            Left err -> putStrLn $ show err