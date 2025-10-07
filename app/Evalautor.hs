module Evalautor where

import Data.Either

import Parsing
import AST
import RuntimeData

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
exec (LetBinding name init) envi = do
    init' <- eval init envi
    let envi' = case envi of
            (Global map)            -> Global ((name, init') : map)
            (Environment map outer) -> Environment ((name, init') : map) outer
    return (envi', return ())
exec (Block stmts) envi = do
    let envi' = Environment [] envi
    (_, io) <- exec_block stmts envi'
    Right (envi, io)
    where
        exec_block :: [Stmt] -> Environment -> Either Error (Environment, IO())
        exec_block [] envi = Right (envi, return ())
        exec_block (stmt:rest) envi = do
            (envi', io) <- exec stmt envi
            (_, io')    <- exec_block rest envi'
            Right (envi, (io >> io'))

exec (Function name parameters body) envi = 
    Right (recEnv, return ())
    where
    -- The closure we bind to `name` must *capture recEnv* (not env).
    func   :: Value
    func   = Function' name parameters body recEnv (length parameters)

    -- New environment frame that binds `name` to its own closure.
    -- IMPORTANT: make a *new* frame; don't dump old bindings into this frame.
    recEnv :: Environment
    recEnv = Environment [(name, func)] envi

eval :: Expr -> Environment -> Either Error Value
eval (Ternary condition then_branch else_branch) envi = do
    condition' <- is_bool envi condition
    if condition' then eval then_branch envi else eval else_branch envi
eval (StringExpr str) _ = Right (String' str)
eval (Name name) envi = find envi name
eval (Number n) _ = Right (Number' n)
eval (Boolean b) _ = Right (Boolean' b)
eval (Binary opp e1 e2) envi
    | opp `elem` [Minus, Multiply, Divide] = binary_number envi opp e1 e2
    | opp `elem` [And, Or] = binary_boolean envi opp e1 e2
    | opp == Plus = plus envi e1 e2
    | opp == Bind = bind envi e1 e2
    | otherwise = relational envi opp e1 e2
    where
        binary_number :: Environment -> BinOpp -> Expr -> Expr -> Either Error Value
        binary_number envi opp e1 e2 = case check_number_opperands envi e1 e2 of
            Left error -> Left error
            Right (n1, n2) -> Right $ Number' $ case opp of
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
        plus :: Environment -> Expr -> Expr -> Either Error Value
        plus envi e1 e2 = do
            e1' <- eval e1 envi
            e2' <- eval e2 envi
            case (e1', e2') of
                (Number' n1, Number' n2) -> return (Number' (n1 + n2))
                (String' s1, String' s2) -> return (String' (s1 ++ s2))
                _ -> Left (Error("cannot add value of type "++(type_of e1')++" and "++(type_of e2')))
    
        bind :: Environment -> Expr -> Expr -> Either Error Value
        bind envi e1 e2 = do
            f <- eval e1 envi
            x <- eval e2 envi
            case f of
                (Lambda' parameters body closure arity) ->
                    if arity <= 1
                        then Left (Error "Cannot bind lambda that only takes 1 arg.")
                        else
                            let closure' = Environment ((head parameters, x) : []) closure
                            in Right (Lambda' (tail parameters) body closure' (arity-1))
                _                -> Left (Error (">< left opperand cannot be of type "++(type_of f)))

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

eval (Lambda parameters body) envi = Right (Lambda' parameters body envi (length parameters))
eval (Call callee args) envi = do
    callee' <- eval callee envi
    case callee' of
        (Function' _ _ _ _ _) -> eval_func_call envi args callee'
        (Lambda' _ _ _ _)     -> eval_call envi args callee'
        _ -> Left (Error ("Cannot call: " ++ (type_of callee')) )
    where
        eval_call :: Environment -> [Expr] -> Value -> Either Error Value
        eval_call envi args (Lambda' parameters body closure arity) = do
            check_arity arity (length args)
            args' <- sequence (map ((flip eval) envi) args)
            let pairs = zip parameters args'
            let envi' = Environment pairs closure
            result <- eval body envi'
            return result
        
        eval_func_call :: Environment -> [Expr] -> Value -> Either Error Value
        eval_func_call envi args (Function' name parameters body closure arity) = do
            check_arity arity (length args)
            args' <- sequence (map ((flip eval) envi) args)
            let pairs = zip parameters args'
            let envi' = Environment pairs closure
            result <- eval body envi'
            return result

        check_arity :: Int -> Int -> Either Error Value
        check_arity expected_arity actual_arity = if expected_arity == actual_arity then Right (Number' 0) else Left (Error ("Exptected an arity of " ++ (show expected_arity) ++ " but got " ++ (show actual_arity)))

eval (LetExpr name init body) envi = do
    value <- eval init envi
    let envi' = Environment [(name, value)] envi
    result <- eval body envi'
    return result

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

type_of :: Value -> String
type_of v = case v of
    String' _ -> "string"
    Number' _  -> "number"
    Boolean'   _  -> "boolean"
    Lambda' _ _ _ _ -> "lambda"