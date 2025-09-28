module Evalautor where

import Parsing

eval :: Expr -> Double
eval (Number n) = n
eval (BinaryOpperation opp n1 n2) = case opp of
    Plus     -> eval n1 + eval n2
    Minus    -> eval n1 - eval n2
    Multiply -> eval n1 * eval n2
    Divide   -> eval n1 / eval n2
eval (UnaryOpperation opp n) =  case opp of
    Negation -> - (eval n)