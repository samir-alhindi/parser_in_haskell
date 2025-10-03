module Main where

import Parsing
import Evalautor

main :: IO ()
main = do
    source <- (readFile "C:\\Users\\Samir\\Desktop\\code\\haskell\\expression_parser\\test.smr")
    case my_parse source of
        Left err   -> print err
        Right e    -> exec e