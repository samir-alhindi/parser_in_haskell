
module AST where

data Stmt =
      Print Expr
    | If BExpr Stmt
    | IfElse BExpr Stmt Stmt
    | Seq [Stmt]
    deriving Show


data Expr =
      AE AExpr
    | BE BExpr
    | StringExpr String
    | Ternary BExpr Expr Expr
    deriving Show

data AExpr =
      Number Double
    | BinaryOpperation ABinOpperator AExpr AExpr
    | UnaryOpperation AUnaryOpperator AExpr
    deriving Show

data BExpr =
      BoolVal Bool
    | And BExpr BExpr
    | Or BExpr BExpr
    | Not BExpr
    | RExpr AExpr AExpr ROpperator
    deriving Show

data ABinOpperator = Plus | Minus | Multiply | Divide deriving Show

data AUnaryOpperator = Negation deriving Show

data ROpperator = Greater | Less | GreaterEqual | LessEqual | DoubleEquals | NotEquals deriving Show