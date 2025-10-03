module Main where

import Parsing
import Evalautor

import Text.Parsec
import Text.Parsec.String

run :: IO ()
run = do
    source <- (readFile "C:\\Users\\Samir\\Desktop\\code\\haskell\\expression_parser\\test.smr")
    case my_parse source of
        Left err   -> print err
        Right e    -> exec e


expre_test :: IO ()
expre_test = do
    source <- (readFile "C:\\Users\\Samir\\Desktop\\code\\haskell\\expression_parser\\test2.smr")
    case parse r_expression "" source of
        Left err   -> print err
        Right e    -> print (b_eval e)

main :: IO ()
main = run
