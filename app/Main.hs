module Main where

import Parsing
import Evalautor

main :: IO ()
main = case parse_source "-(1+2)*3" of
    Left error -> print error
    Right ast -> print (eval ast)