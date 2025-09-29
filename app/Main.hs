module Main where

import Parsing
import Evalautor

run :: String -> IO()
run source = case my_parse source of
    Left err   -> print err
    Right e    -> print (eval e)

main :: IO ()
main = do
    run " not (false and false)"
    run "1+2"
    run "if false then true else 1+1"
    run "if false then 1 else if false then 2 else if true then 3 else 4"


