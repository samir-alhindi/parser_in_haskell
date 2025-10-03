module Evalautor where

import Parsing
import AST

data Value = 
      Number' {get_num :: Double}
    | Bool' {get_bool :: Bool}
    | String' {get_str :: String}

instance Show Value where
    show (Number' n) = show n
    show (Bool' b  ) = show b
    show (String' s) = show s

exec :: Stmt -> IO ()
exec (Print expre)                              = print (eval expre)
exec (If condition then_branch)                 = if get_bool (b_eval condition) then exec then_branch else return ()
exec (IfElse condition then_branch else_branch) = if get_bool (b_eval condition) then exec then_branch else exec else_branch
exec (Seq list)                               =   mapM_ exec list

eval :: Expr -> Value
eval (AE e) =  (a_eval e)
eval (BE e) =  (b_eval e) 
eval (Ternary condition then_branch else_branch) = if get_bool (b_eval condition) then eval then_branch else eval else_branch
eval (StringExpr str) = (String' str)

a_eval :: AExpr -> Value
a_eval (Number n) = Number' n
a_eval (BinaryOpperation opp n1 n2) = case opp of
    Plus     -> Number' (get_num (a_eval n1) + get_num (a_eval n2))
    Minus    -> Number' (get_num (a_eval n1) - get_num (a_eval n2))
    Multiply -> Number' (get_num (a_eval n1) * get_num (a_eval n2))
    Divide   -> Number' (get_num (a_eval n1) / get_num (a_eval n2))
a_eval (UnaryOpperation opp n) =  case opp of
    Negation -> Number' ( - (get_num (a_eval n)))


b_eval :: BExpr -> Value
b_eval (BoolVal b) = Bool' b
b_eval (And b1 b2) = Bool' (get_bool (b_eval b1) && get_bool (b_eval b2))
b_eval (Or b1 b2)  = Bool' (get_bool (b_eval b1) || get_bool (b_eval b2))
b_eval (Not b)     = Bool' (not (get_bool (b_eval b)))
b_eval r_expr      = r_eval r_expr


r_eval :: BExpr -> Value
r_eval (RExpr a1 a2 opp) = 
    let (n1, n2) = (get_num (a_eval a1), get_num (a_eval a2)) in
        case opp of
            Greater         -> Bool' (n1 > n2)
            Less            -> Bool' (n1 < n2)
            GreaterEqual    -> Bool' (n1 >= n2)
            LessEqual       -> Bool' (n1 <= n2)
            DoubleEquals    -> Bool' (n1 == n2)
            NotEquals       -> Bool' (n1 /= n2)