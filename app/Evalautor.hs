module Evalautor where

import AST
import RuntimeData
import Text.Parsec
 
exec_program :: [Stmt] -> Either Error' (IO ())
exec_program program = helper program (Global [])
    where
        helper :: [Stmt] -> Environment -> Either Error' (IO ())
        helper [] _ = Right (return ())
        helper (stmt:rest) envi = do
            (envi', io) <- exec stmt envi
            io'         <- helper rest envi'
            return (io >> io')
 
exec ::  Stmt -> Environment -> Either Error' (Environment, IO())
exec (Print expr) envi = do
    result <- eval expr envi
    return (envi, print result)

exec (If pos condition then_branch) envi = do
    condition' <- is_bool envi condition pos
    if condition'
        then exec then_branch envi
        else Right (envi, return ())

exec (IfElse pos condition then_branch else_branch) envi = do
    condition' <- is_bool envi condition pos
    if condition'
        then exec then_branch envi
        else exec else_branch envi

exec (LetBinding name init') envi = do
    init'' <- eval init' envi
    let envi' = extend_envi envi (name, init'')
    return (envi', return ())

exec (Block stmts) envi = do
    let envi' = Environment [] envi
    (_, io) <- exec_block stmts envi'
    Right (envi, io)
    where
        exec_block :: [Stmt] -> Environment -> Either Error' (Environment, IO())
        exec_block [] envi'' = Right (envi'', return ())
        exec_block (stmt:rest) envi'' = do
            (envi''', io) <- exec stmt envi''
            (_, io')    <- exec_block rest envi'''
            Right (envi'', (io >> io'))

exec (Function name parameters body) envi = 
    Right (recEnv, return ())
        where
            recEnv :: Environment
            recEnv = Environment [(name, func)] envi

            func   :: Value
            func   = Function' name parameters body recEnv (length parameters)

eval :: Expr -> Environment -> Either Error' Value
eval (Ternary pos condition then_branch else_branch) envi = do
    condition' <- is_bool envi condition pos
    if condition' then eval then_branch envi else eval else_branch envi

eval (StringExpr str) _ = Right (String' str)
eval (Name pos name) envi = find envi name pos
eval (Number n) _ = Right (Number' n)
eval (Boolean b) _ = Right (Boolean' b)

eval (Binary pos opp e1 e2) envi
    | opp `elem` [Minus, Multiply, Divide] = binary_number
    | opp `elem` [And, Or]                 = binary_boolean
    | opp == Plus                          = plus
    | opp == Bind                          = bind
    | otherwise                            = relational
    where
        binary_number :: Either Error' Value
        binary_number = do
            (n1, n2) <- check_number_opperands
            Right $ Number' $ case opp of
                Minus    -> n1 - n2
                Multiply -> n1 * n2
                Divide   -> n1 / n2
                _        -> -1 -- This will never run.
        
        binary_boolean :: Either Error' Value
        binary_boolean = do
            (b1, b2) <- check_boolean_opperands
            Right $ Boolean' $ case opp of
                And -> b1 && b2
                Or  -> b1 || b2
                _   -> False -- This will never run.
        
        relational :: Either Error' Value
        relational
            | opp `elem` [DoubleEquals, NotEquals] = do
                    n1 <- eval e1 envi
                    n2 <- eval e2 envi
                    Right $ Boolean' $ case opp of
                        DoubleEquals -> n1 == n2
                        NotEquals    -> n1 /= n2
                        _            -> False -- This will never run.

            | opp `elem` [Greater, Less, GreaterEqual, LessEqual] = do
                (n1, n2) <- check_number_opperands
                Right $ Boolean' $ case opp of
                    Less         -> n1 < n2
                    Greater      -> n1 > n2
                    GreaterEqual -> n1 >= n2
                    LessEqual    -> n1 <= n2
                    _            -> False -- This will never run.

            | otherwise = Left (Error' "This will never run." pos)

        plus :: Either Error' Value
        plus = do
            e1' <- eval e1 envi
            e2' <- eval e2 envi
            case (e1', e2') of
                (Number' n1, Number' n2) -> return (Number' (n1 + n2))
                (String' s1, String' s2) -> return (String' (s1 ++ s2))
                _ -> Left (Error'("cannot add value of types "++(type_of e1')++" and "++(type_of e2')) pos)
    
        bind :: Either Error' Value
        bind = do
            f <- eval e1 envi
            x <- eval e2 envi
            case f of
                (Lambda'        parameters body closure arity) -> helper x (Lambda')        parameters body closure arity
                (Function' name parameters body closure arity) -> helper x (Function' name) parameters body closure arity
                _ -> Left (Error' ("Left '><' opperand must be a callable and not of type " ++ (type_of f)) pos)
 
            where
                helper :: Value
                        -> ([String] -> Expr -> Environment -> Int -> Value)
                        -> [String] -> Expr -> Environment -> Int -> Either Error' Value
                helper x callable parameters body closure arity =
                    if arity == 1
                        then eval (Call pos e1 [e2]) envi
                        else
                            let closure' = Environment ((head parameters, x) : []) closure
                            in Right (callable (tail parameters) body closure' (arity-1))

        check_number_opperands :: Either Error' (Double, Double)
        check_number_opperands = do
            n1 <- eval e1 envi
            n2 <- eval e2 envi
            (n1', n2') <- helper (n1, n2)
            return (n1', n2')
            where
                helper :: (Value, Value) -> Either Error' (Double, Double)
                helper (Number' n1, Number' n2) = Right (n1, n2)
                helper _ = Left (Error' "Both opperands must be numbers." pos)

        check_boolean_opperands :: Either Error' (Bool, Bool)
        check_boolean_opperands = do
            b1 <- eval e1 envi
            b2 <- eval e2 envi
            (b1', b2') <- helper (b1, b2)
            return (b1', b2')
            where
                helper :: (Value, Value) -> Either Error' (Bool, Bool)
                helper (Boolean' b1, Boolean' b2) = Right (b1, b2)
                helper _ = Left (Error' "Both opperands must be booleans." pos)


eval (Unary pos opp e) envi = do
    v <- eval e envi
    case opp of
        Negation ->
            case v of
            Number' n -> Right (Number' (-n))
            _ -> Left (Error' ("'-' opperator must be a number and not of type "++(type_of v)) pos)
        Not ->
            case v of
            Boolean' b -> Right (Boolean' (not b))
            _ -> Left (Error' ("'not' opperand must be a boolean and not of type "++(type_of v)) pos)

eval (Lambda parameters body) envi = Right (Lambda' parameters body envi (length parameters))
 
eval (Call pos callee args) envi = do
    callee' <- eval callee envi
    case callee' of
        (Function' _ parameters body closure arity) -> eval_call parameters body closure arity
        (Lambda'     parameters body closure arity) -> eval_call parameters body closure arity
        _                                           -> Left (Error' ("Cannot call: " ++ (type_of callee')) pos)
    where
        eval_call :: [String] -> Expr -> Environment -> Int -> Either Error' Value
        eval_call parameters body closure arity  = do
            check_arity arity (length args)
            args' <- sequence (map ((flip eval) envi) args)
            let pairs = zip parameters args'
            let envi' = Environment pairs closure
            result <- eval body envi'
            return result

        check_arity :: Int -> Int -> Either Error' ()
        check_arity expected_arity actual_arity = if expected_arity == actual_arity then Right () else Left (Error' ("Exptected an arity of " ++ (show expected_arity) ++ " but got " ++ (show actual_arity)) pos)

eval (LetExpr name init' body) envi = do
    value <- eval init' envi
    let envi' = Environment [(name, value)] envi
    result <- eval body envi'
    return result

eval (List elements) envi = do
    elements' <- sequence (map (`eval` envi) elements)
    return (List' elements')

is_bool :: Environment -> Expr -> SourcePos -> Either Error' Bool
is_bool envi expr pos = case eval expr envi of
    Right (Boolean' b) -> Right b
    Left err -> Left err
    _         -> Left (Error' "condition must be a boolean value." pos)

type_of :: Value -> String
type_of v = case v of
    String' _           -> "string"
    Number' _           -> "number"
    Boolean'   _        -> "boolean"
    Lambda' _ _ _ _     -> "lambda"
    Function' _ _ _ _ _ -> "function"
    List' _             -> "list"