
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
    | Name String
    | Binary BinOpp Expr Expr
    | Unary UnaryOpp Expr
    | StringExpr String
    | Ternary Expr Expr Expr
    | Lambda [String] Expr
    | Call Expr [Expr]
    deriving (Show, Eq)

data BinOpp = Plus | Minus | Multiply | Divide
  | And | Or
  | Greater | Less | GreaterEqual | LessEqual | DoubleEquals | NotEquals
  | Bind
  deriving (Show, Eq)

data UnaryOpp = Negation | Not deriving (Show, Eq)