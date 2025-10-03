module Evalautor where

import Parsing
import AST

data Value = 
      Number' {get_num :: Double}
    | Boolean' {get_bool :: Bool}
    | String' {get_str :: String}
    deriving (Eq)

instance Show Value where
    show (Number' n) = show n
    show (Boolean' b  ) = show b
    show (String' s) = show s

newtype Error = Error {get_log :: String}

instance Show Error where
    show (Error s) = "Error: " ++ s

exec :: Stmt -> Either Error (IO ())
exec (Print expre) = print <$> (eval expre)
exec (If condition then_branch) = case is_bool condition of
    Right b -> if b then exec then_branch else Right (return ())
    Left err -> Left err
exec (IfElse condition then_branch else_branch) = case is_bool condition of
    Right b -> if b then exec then_branch else exec else_branch
    Left err -> Left err
exec (Seq list) = do
    ios <- mapM exec list
    Right (sequence_ ios)

eval :: Expr -> Either Error Value
eval (Ternary condition then_branch else_branch) = case is_bool condition of
    Right b  -> if b then eval then_branch else eval else_branch
    Left err -> Left err
eval (StringExpr str) = Right (String' str)
eval (Number n) = Right (Number' n)
eval (Boolean b)   = Right (Boolean' b)
eval (Binary opp e1 e2) 
    | opp `elem` [Plus, Minus, Multiply, Divide] = binary_number opp e1 e2
    | opp `elem` [And, Or] = binary_boolean opp e1 e2
    | otherwise = relational opp e1 e2
    where
        binary_number :: BinOpp -> Expr -> Expr -> Either Error Value
        binary_number opp e1 e2 = case check_number_opperands e1 e2 of
            Left error -> Left error
            Right (n1, n2) -> Right $ Number' $ case opp of
                Plus -> n1 + n2
                Minus -> n1 - n2
                Multiply -> n1 * n2
                Divide -> n1 / n2
        
        binary_boolean :: BinOpp -> Expr -> Expr -> Either Error Value
        binary_boolean opp e1 e2 = case check_boolean_opperands e1 e2 of
            Left error     -> Left error
            Right (b1, b2) -> Right $ Boolean' $ case opp of
                And -> b1 && b2
                Or  -> b1 || b2
        
        relational :: BinOpp -> Expr -> Expr -> Either Error Value
        relational opp e1 e2
            | opp `elem` [DoubleEquals, NotEquals] = case opp of
                DoubleEquals -> do
                    n1 <- eval e1
                    n2 <- eval e2
                    return (Boolean' (n1 == n2))
                NotEquals   -> do
                    n1 <- eval e1
                    n2 <- eval e2
                    return (Boolean' (n1 /= n2))
            | opp `elem` [Greater, Less, GreaterEqual, LessEqual] = case check_number_opperands e1 e2 of
                Left error -> Left error
                Right (n1, n2) -> Right $ Boolean' $ case opp of
                    Less -> n1 < n2
                    Greater -> n1 > n2
                    GreaterEqual -> n1 >= n2
                    LessEqual -> n1 <= n2
eval (Unary opp e) = unary opp e
    where
        unary :: UnaryOpp -> Expr -> Either Error Value
        unary opp e = case opp of
            Negation -> case eval e of
                Right (Number' n) -> Right (Number' (-n))
                Left err -> Left err 
                _ -> Left (Error ("'-' opperator must be a number."))
            Not -> case eval e of
                Right (Boolean' b) -> Right (Boolean' (not b))
                Left err -> Left err
                _ -> Left (Error "not opperand must be a boolean.")

is_bool :: Expr -> Either Error Bool
is_bool expr = case eval expr of
    Right (Boolean' b) -> Right b
    Left err -> Left err
    _         -> Left (Error "Must be a boolean value.")

check_number_opperands :: Expr -> Expr -> Either Error (Double, Double)
check_number_opperands e1 e2 = do
    n1 <- eval e1
    n2 <- eval e2
    (n1', n2') <- helper (n1, n2)
    return (n1', n2')
    where
        helper :: (Value, Value) -> Either Error (Double, Double)
        helper (Number' n1, Number' n2) = Right (n1, n2)
        helper _ = Left (Error "Both opperands must be numbers.")

check_boolean_opperands :: Expr -> Expr -> Either Error (Bool, Bool)
check_boolean_opperands e1 e2 = do
    b1 <- eval e1
    b2 <- eval e2
    (b1', b2') <- helper (b1, b2)
    return (b1', b2')
     
    where
        helper :: (Value, Value) -> Either Error (Bool, Bool)
        helper (Boolean' b1, Boolean' b2) = Right (b1, b2)
        helper _ = Left (Error "Both opperands must be booleans.")