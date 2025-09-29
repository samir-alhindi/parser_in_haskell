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


