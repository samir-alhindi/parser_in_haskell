
module AST where

data Stmt =
      Print Expr
    | If Expr Stmt
    | IfElse Expr Stmt Stmt
    | Seq [Stmt]
    deriving Show

data Expr =
      Number Double
    | Boolean Bool
    | Binary BinOpp Expr Expr
    | Unary UnaryOpp Expr
    | StringExpr String
    | Ternary Expr Expr Expr
    deriving Show

data BinOpp = Plus | Minus | Multiply | Divide
  | And | Or
  |Greater | Less | GreaterEqual | LessEqual | DoubleEquals | NotEquals
  deriving (Show, Eq)

data UnaryOpp = Negation | Not deriving (Show, Eq)