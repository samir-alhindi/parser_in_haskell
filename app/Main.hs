module Main where

import Parsing
import Evalautor

import Text.Parsec

run :: IO ()
run = do
    source <- (readFile "C:\\Users\\Samir\\Desktop\\code\\haskell\\expression_parser\\test3.smr")
    case my_parse source of
        Left err   -> print err
        Right e    -> case exec e of
            Left err -> print err
            Right io -> io

expre_test :: IO ()
expre_test = do
    source <- (readFile "C:\\Users\\Samir\\Desktop\\code\\haskell\\expression_parser\\test2.smr")
    case parse expression "" source of
        Left err   -> print err
        Right e    -> print (eval e)

main :: IO ()
main = run
