module Evalautor where

import Parsing
import AST
import Environment

instance Show Value where
    show (Number' n) = show n
    show (Boolean' b  ) = show b
    show (String' s) = show s

instance Show Error where
    show (Error s) = "Error: " ++ s

exec_program :: [Stmt] -> Either Error (IO ())
exec_program program = helper program (Global [])
    where
        helper :: [Stmt] -> Environment -> Either Error (IO ())
        helper [] _ = Right (return ())
        helper (stmt:rest) envi = do
            (envi', io) <- exec stmt envi
            io'         <- helper rest envi'
            return (io >> io')

exec ::  Stmt -> Environment -> Either Error (Environment, IO())
exec (Print expr) envi = do
    result <- eval expr envi
    return (envi, print result)
exec (If condition then_branch) envi = do
    condition' <- is_bool envi condition
    if condition' then exec then_branch envi else Right (envi, return ())
exec (IfElse condition then_branch else_branch) envi = do
    condition' <- is_bool envi condition
    if condition' then exec then_branch envi else exec else_branch envi
exec (VarDeclre name init) envi = do
    init' <- eval init envi
    let envi' = case envi of
            (Global map)            -> Global ((name, init') : map)
            (Environment map outer) -> Environment ((name, init') : map) outer
    return (envi', return ())

eval :: Expr -> Environment -> Either Error Value
eval (Ternary condition then_branch else_branch) envi = do
    condition' <- is_bool envi condition
    if condition' then eval then_branch envi else eval else_branch envi
eval (StringExpr str) _ = Right (String' str)
eval (Variable name) envi = find envi name
eval (Number n) _ = Right (Number' n)
eval (Boolean b) _ = Right (Boolean' b)
eval (Binary opp e1 e2) envi
    | opp `elem` [Plus, Minus, Multiply, Divide] = binary_number envi opp e1 e2
    | opp `elem` [And, Or] = binary_boolean envi opp e1 e2
    | otherwise = relational envi opp e1 e2
    where
        binary_number :: Environment -> BinOpp -> Expr -> Expr -> Either Error Value
        binary_number envi opp e1 e2 = case check_number_opperands envi e1 e2 of
            Left error -> Left error
            Right (n1, n2) -> Right $ Number' $ case opp of
                Plus -> n1 + n2
                Minus -> n1 - n2
                Multiply -> n1 * n2
                Divide -> n1 / n2
        
        binary_boolean :: Environment -> BinOpp -> Expr -> Expr -> Either Error Value
        binary_boolean envi opp e1 e2 = case check_boolean_opperands envi e1 e2 of
            Left error     -> Left error
            Right (b1, b2) -> Right $ Boolean' $ case opp of
                And -> b1 && b2
                Or  -> b1 || b2
        
        relational :: Environment -> BinOpp -> Expr -> Expr -> Either Error Value
        relational envi opp e1 e2
            | opp `elem` [DoubleEquals, NotEquals] = case opp of
                DoubleEquals -> do
                    n1 <- eval e1 envi
                    n2 <- eval e2 envi
                    return (Boolean' (n1 == n2))
                NotEquals   -> do
                    n1 <- eval e1 envi
                    n2 <- eval e2 envi
                    return (Boolean' (n1 /= n2))
            | opp `elem` [Greater, Less, GreaterEqual, LessEqual] = case check_number_opperands envi e1 e2 of
                Left error -> Left error
                Right (n1, n2) -> Right $ Boolean' $ case opp of
                    Less -> n1 < n2
                    Greater -> n1 > n2
                    GreaterEqual -> n1 >= n2
                    LessEqual -> n1 <= n2
eval (Unary opp e) envi = unary envi opp e
    where
        unary :: Environment -> UnaryOpp -> Expr -> Either Error Value
        unary envi opp e = case opp of
            Negation -> case eval e envi of
                Right (Number' n) -> Right (Number' (-n))
                Left err -> Left err 
                _ -> Left (Error ("'-' opperator must be a number."))
            Not -> case eval e envi of
                Right (Boolean' b) -> Right (Boolean' (not b))
                Left err -> Left err
                _ -> Left (Error "not opperand must be a boolean.")

is_bool :: Environment -> Expr -> Either Error Bool
is_bool envi expr = case eval expr envi of
    Right (Boolean' b) -> Right b
    Left err -> Left err
    _         -> Left (Error "Must be a boolean value.")

check_number_opperands :: Environment -> Expr -> Expr -> Either Error (Double, Double)
check_number_opperands envi e1 e2 = do
    n1 <- eval e1 envi
    n2 <- eval e2 envi
    (n1', n2') <- helper (n1, n2)
    return (n1', n2')
    where
        helper :: (Value, Value) -> Either Error (Double, Double)
        helper (Number' n1, Number' n2) = Right (n1, n2)
        helper _ = Left (Error "Both opperands must be numbers.")

check_boolean_opperands :: Environment -> Expr -> Expr -> Either Error (Bool, Bool)
check_boolean_opperands envi e1 e2 = do
    b1 <- eval e1 envi
    b2 <- eval e2 envi
    (b1', b2') <- helper (b1, b2)
    return (b1', b2')
    where
        helper :: (Value, Value) -> Either Error (Bool, Bool)
        helper (Boolean' b1, Boolean' b2) = Right (b1, b2)
        helper _ = Left (Error "Both opperands must be booleans.")