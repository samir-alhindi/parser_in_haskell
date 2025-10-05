
module AST where

data Stmt =
      Print Expr
    | If Expr Stmt
    | IfElse Expr Stmt Stmt
    | LetBinding String Expr
    | Block [Stmt]
    deriving Show

data Expr =
      Number Double
    | Boolean Bool
    | Variable String
    | Binary BinOpp Expr Expr
    | Unary UnaryOpp Expr
    | StringExpr String
    | Ternary Expr Expr Expr
    deriving Show

data BinOpp = Plus | Minus | Multiply | Divide
  | And | Or
  | Greater | Less | GreaterEqual | LessEqual | DoubleEquals | NotEquals
  deriving (Show, Eq)

data UnaryOpp = Negation | Not deriving (Show, Eq)

data Value = 
      Number' {get_num :: Double}
    | Boolean' {get_bool :: Bool}
    | String' {get_str :: String}
    deriving (Eq)

newtype Error = Error {get_log :: String}