module Evalautor where

import Parsing

eval :: Expr -> Either Double Bool
eval (AE e) = Left  (a_eval e)
eval (BE e) = Right (b_eval e)  

a_eval :: AExpr -> Double
a_eval (Number n) = n
a_eval (BinaryOpperation opp n1 n2) = case opp of
    Plus     -> a_eval n1 + a_eval n2
    Minus    -> a_eval n1 - a_eval n2
    Multiply -> a_eval n1 * a_eval n2
    Divide   -> a_eval n1 / a_eval n2
a_eval (UnaryOpperation opp n) =  case opp of
    Negation -> - (a_eval n)

b_eval :: BExpr -> Bool
b_eval (BoolVal b) = b
b_eval (And b1 b2) = b_eval b1 && b_eval b2
b_eval (Or b1 b2) = b_eval b1 || b_eval b2 
b_eval (Not b) = not (b_eval b)