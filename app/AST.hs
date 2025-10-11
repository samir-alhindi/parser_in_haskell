
module AST where

import Text.Parsec

data Stmt =
      Print Expr
    | If SourcePos Expr Stmt
    | IfElse SourcePos Expr Stmt Stmt
    | LetBinding String Expr
    | Function String [String] Expr
    | Block [Stmt]
    deriving Show

data Expr =
      Number Double
    | Boolean Bool
    | Name SourcePos String
    | LetExpr String Expr Expr
    | Binary SourcePos BinOpp Expr Expr
    | Unary SourcePos UnaryOpp Expr
    | StringExpr String
    | Ternary SourcePos Expr Expr Expr
    | Lambda [String] Expr
    | Call SourcePos Expr [Expr]
    | List [Expr]
    deriving (Show, Eq)


data BinOpp = Plus | Minus | Multiply | Divide
  | And | Or
  | Greater | Less | GreaterEqual | LessEqual | DoubleEquals | NotEquals
  | Bind | CallOp
  deriving (Show, Eq)

data UnaryOpp = Negation | Not deriving (Show, Eq)