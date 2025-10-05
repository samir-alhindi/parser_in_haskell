module Environment where

import AST

type Environment = [(String, Value)]

find :: Environment -> String -> Either Error Value
find envi name = case filter p envi of
    []   -> Left (Error ("unbound variable: " ++ name))
    list -> let (_, value) = head list in Right value
    where
        p :: (String, Value) -> Bool
        p (k, _) = k == name
    