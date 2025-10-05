module Main where

import Parsing
import Evalautor

import Text.Parsec

run :: IO ()
run = do
    source <- (readFile "C:\\Users\\Samir\\Desktop\\code\\haskell\\expression_parser\\test_var.smr")
    case my_parse source of
        Left err         -> print err
        Right program    -> case exec_program program of
            Left err -> print err
            Right io -> io

main :: IO ()
main = run
