module Evalautor where

import Parsing
import AST

data Value = 
      Number' {get_num :: Double}
    | Bool' {get_bool :: Bool}
    | String' {get_str :: String}
    | RuntimeError {get_log :: String}
    deriving (Eq)

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
r_eval (RExpr e1 e2 opp)
            | opp `elem` [Greater, Less, GreaterEqual, LessEqual] = if check_number_opperands e1 e2 then eval_numbers e1 e2 opp else RuntimeError "Error, Both opperands must be numbers"
            | opp `elem` [DoubleEquals, NotEquals]                = equality e1 e2 opp
    where
        eval_numbers :: Expr -> Expr -> ROpperator -> Value
        eval_numbers e1 e2 opp =
            let (n1, n2) = (get_num (eval e1), get_num (eval e2)) in
                Bool' $ case opp of
                    Greater      -> n1 > n2
                    Less         -> n1 < n2
                    GreaterEqual -> n1 >= n2
                    LessEqual    -> n1 <= n2
        
        equality :: Expr -> Expr -> ROpperator -> Value
        equality e1 e2 opp = let (v1, v2) = (eval e1, eval e2) in
            Bool' $ case opp of
                DoubleEquals -> v1 == v2
                NotEquals    -> v1 /= v2


check_number_opperands :: Expr -> Expr -> Bool
check_number_opperands e1 e2 = (helper (eval e1)) && (helper (eval e2))
    where
        helper :: Value -> Bool
        helper (Number' _) = True
        _                  = False