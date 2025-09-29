module Main where

import Parsing
import Evalautor
import Text.Parsec

a_run :: String -> IO()
a_run source = case a_parse source of
    Left error -> print error
    Right ast -> print (a_eval ast)

b_run :: String -> IO()
b_run source = case b_parse source of
    Left error -> print error
    Right ast -> print (b_eval ast)

main :: IO ()
main = b_run " not (false and false)"

