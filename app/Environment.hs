module Environment where

import AST

type Map = [(String, Value)]

data Environment =
      Global {get_map :: Map}
    | Environment {get_map :: Map, get_outer :: Environment}

find :: Environment -> String -> Either Error Value
find envi name = case envi of
    (Global map) -> case filter p map of
        []   -> Left (Error ("unbound variable: " ++ name))
        list -> let (_, value) = head list in Right value
    (Environment map outer) -> case filter p map of
        []   -> find outer name
        list -> let (_, value) = head list in Right value
    where
        p :: (String, Value) -> Bool
        p (k, _) = k == name
    